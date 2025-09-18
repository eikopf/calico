//! A hashtable with mixed static-dynamic keys.

use std::{
    convert::Infallible,
    fmt::Debug,
    hash::{BuildHasher, Hash, Hasher, RandomState},
};

pub use hashbrown::hash_table::{Entry, OccupiedEntry, VacantEntry};

use hashbrown::HashTable;
use winnow::stream::Stream;

use crate::parser::escaped::{Equiv, Escaped};

#[derive(Clone)]
pub struct Table<K1, K2, V1, V2> {
    raw_table: HashTable<Item<K1, K2, V1, V2>>,
    state: RandomState,
}

impl<K1: Debug, K2: Debug, V1: Debug, V2: Debug> Debug for Table<K1, K2, V1, V2> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Table").field(&self.raw_table).finish()
    }
}

impl<K1, K2, V1, V2> Default for Table<K1, K2, V1, V2> {
    fn default() -> Self {
        Self {
            raw_table: Default::default(),
            state: Default::default(),
        }
    }
}

impl<K1, K2, V1, V2> Table<K1, K2, V1, V2> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            raw_table: HashTable::with_capacity(capacity),
            state: Default::default(),
        }
    }

    pub fn capacity(&self) -> usize {
        self.raw_table.capacity()
    }

    pub fn len(&self) -> usize {
        self.raw_table.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw_table.is_empty()
    }
}

impl<K1, K2, V1, V2> Table<K1, K2, V1, V2>
where
    K1: Hash + PartialEq,
    K2: HashCaseless,
{
    pub fn contains_key<T: HashCaseless + Equiv<K2>>(&self, key: &Key<K1, T>) -> bool {
        self.get(key).is_some()
    }

    pub fn contains_known_key(&self, key: K1) -> bool {
        self.get_known(key).is_some()
    }

    pub fn get<'a, T: HashCaseless + Equiv<K2>>(
        &'a self,
        key: &Key<K1, T>,
    ) -> Option<&'a Item<K1, K2, V1, V2>> {
        self.raw_table.find(self.state.hash_one(key), |rhs| {
            key.as_ref().eq(&rhs.as_key())
        })
    }

    pub fn get_known(&self, key: K1) -> Option<&V1> {
        self.get(&Key::known(key))
            .map(|item| item.as_known().unwrap().1)
    }

    pub fn get_mut<'a, T: HashCaseless + Equiv<K2>>(
        &'a mut self,
        key: &Key<K1, T>,
    ) -> Option<&'a mut Item<K1, K2, V1, V2>> {
        self.raw_table.find_mut(self.state.hash_one(key), |rhs| {
            key.as_ref().eq(&rhs.as_key())
        })
    }

    pub fn get_known_mut(&mut self, key: K1) -> Option<&mut V1> {
        self.get_mut(&Key::known(key))
            .map(|item| item.as_known_mut().unwrap().1)
    }

    pub fn insert_known(&mut self, key: K1, value: V1) -> Option<Item<K1, K2, V1, V2>>
    where
        K2: Equiv,
    {
        self.insert(Item::Known { key, value })
    }

    pub fn insert_unknown(&mut self, key: K2, value: V2) -> Option<Item<K1, K2, V1, V2>>
    where
        K2: Equiv,
    {
        self.insert(Item::Unknown { key, value })
    }

    pub(crate) fn append_unknown<V>(
        &mut self,
        key: K2,
        value: V,
        append: impl for<'a> FnOnce(&'a mut V2, V),
        init: impl FnOnce() -> V2,
    ) where
        K2: Equiv,
    {
        match self.entry(KeyRef::Unknown(&key)) {
            Entry::Occupied(mut entry) => {
                let values = entry.get_mut().as_unknown_mut().unwrap().1;
                append(values, value);
            }
            Entry::Vacant(entry) => {
                let mut values = init();
                append(&mut values, value);
                entry.insert(Item::Unknown { key, value: values });
            }
        }
    }

    pub fn insert(&mut self, item: Item<K1, K2, V1, V2>) -> Option<Item<K1, K2, V1, V2>>
    where
        K2: Equiv,
    {
        match self.entry(item.as_key()) {
            Entry::Occupied(mut entry) => Some(std::mem::replace(entry.get_mut(), item)),
            Entry::Vacant(entry) => {
                entry.insert(item);
                None
            }
        }
    }

    pub fn remove<T: HashCaseless + Equiv<K2>>(
        &mut self,
        key: &Key<K1, T>,
    ) -> Option<Item<K1, K2, V1, V2>> {
        match self.entry(key.as_ref()) {
            Entry::Occupied(entry) => {
                let (item, _entry) = entry.remove();
                Some(item)
            }
            Entry::Vacant(_) => None,
        }
    }

    pub fn remove_known(&mut self, key: K1) -> Option<V1> {
        self.remove(&Key::known(key))
            .map(|item| item.into_known().unwrap().1)
    }

    pub fn entry<'a, T: HashCaseless + Equiv<K2>>(
        &'a mut self,
        key: KeyRef<'_, K1, T>,
    ) -> Entry<'a, Item<K1, K2, V1, V2>> {
        self.raw_table.entry(
            self.state.hash_one(&key),
            |rhs| key.eq(&rhs.as_key()),
            |entry| self.state.hash_one(entry),
        )
    }
}

/// A key into a [`Table`], which may either be [`Known`] or [`Unknown`]. The [`Unknown`] variant
/// is treated as iCalendar source text (e.g. the name of a property or component), and as such is
/// considered to be case-and-line-fold-insensitive. In practice, this means that the
/// implementations of [`PartialEq`] and [`Hash`] require that `K2` implement [`Equiv`] and
/// [`HashCaseless`] respectively.
///
/// [`Known`]: Key::Known
/// [`Unknown`]: Key::Unknown
#[derive(Debug, Clone)]
pub enum Key<K1, K2> {
    /// A known key value.
    Known(K1),
    /// An unknown key value.
    Unknown(K2),
}

/// The borrowed equivalent of a [`Key`], usually obtained from [`Key::as_ref`].
#[derive(Debug, Clone, Copy)]
pub enum KeyRef<'a, K1: ?Sized, K2: ?Sized> {
    Known(&'a K1),
    Unknown(&'a K2),
}

impl<K1: Hash, K2: HashCaseless> Hash for Key<K1, K2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<K1, KL, KR> PartialEq<Key<K1, KR>> for Key<K1, KL>
where
    K1: PartialEq,
    KL: Equiv<KR>,
{
    fn eq(&self, other: &Key<K1, KR>) -> bool {
        self.as_ref().eq(&other.as_ref())
    }
}

impl<K1, K2> Key<K1, K2> {
    pub const fn as_ref(&self) -> KeyRef<'_, K1, K2> {
        match self {
            Key::Known(k) => KeyRef::Known(k),
            Key::Unknown(k) => KeyRef::Unknown(k),
        }
    }
}

impl<K> Key<K, Infallible> {
    pub const fn known(key: K) -> Self {
        Key::Known(key)
    }
}

impl<K1: ?Sized + Hash, K2: ?Sized + HashCaseless> Hash for KeyRef<'_, K1, K2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            KeyRef::Known(k) => k.hash(state),
            KeyRef::Unknown(k) => k.hash_caseless(state),
        }
    }
}

impl<'a, K1, KL, KR> PartialEq<KeyRef<'a, K1, KR>> for KeyRef<'a, K1, KL>
where
    K1: ?Sized + PartialEq,
    KL: ?Sized + Equiv<KR>,
    KR: ?Sized,
{
    fn eq(&self, other: &KeyRef<'a, K1, KR>) -> bool {
        match (self, other) {
            (Self::Known(l), KeyRef::Known(r)) => l == r,
            (Self::Unknown(l), KeyRef::Unknown(r)) => l.equiv(r),
            _ => false,
        }
    }
}

/// An item in a [`Table`], which may be [`Known`] or [`Unknown`]. Each variant is a key-value
/// pair, with the [`Unknown`] variant subject to the same insensitivity requirements as
/// [`Key::Unknown`]. No requirements are imposed on the value types.
///
/// [`Known`]: Item::Known
/// [`Unknown`]: Item::Unknown
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item<K1, K2, V1, V2> {
    Known { key: K1, value: V1 },
    Unknown { key: K2, value: V2 },
}

impl<K1: Hash, K2: HashCaseless, V1, V2> Hash for Item<K1, K2, V1, V2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_key().hash(state)
    }
}

impl<K1, K2, V1, V2> Item<K1, K2, V1, V2> {
    pub const fn as_key(&self) -> KeyRef<'_, K1, K2> {
        match self {
            Item::Known { key, .. } => KeyRef::Known(key),
            Item::Unknown { key, .. } => KeyRef::Unknown(key),
        }
    }

    pub const fn as_known(&self) -> Option<(&K1, &V1)> {
        match self {
            Item::Known { key, value } => Some((key, value)),
            Item::Unknown { .. } => None,
        }
    }

    pub const fn as_unknown(&self) -> Option<(&K2, &V2)> {
        match self {
            Item::Unknown { key, value } => Some((key, value)),
            Item::Known { .. } => None,
        }
    }

    pub const fn as_known_mut(&mut self) -> Option<(&mut K1, &mut V1)> {
        match self {
            Item::Known { key, value } => Some((key, value)),
            Item::Unknown { .. } => None,
        }
    }

    pub const fn as_unknown_mut(&mut self) -> Option<(&mut K2, &mut V2)> {
        match self {
            Item::Unknown { key, value } => Some((key, value)),
            Item::Known { .. } => None,
        }
    }

    pub fn into_known(self) -> Option<(K1, V1)> {
        match self {
            Item::Known { key, value } => Some((key, value)),
            Item::Unknown { .. } => None,
        }
    }

    pub fn into_unknown(self) -> Option<(K2, V2)> {
        match self {
            Item::Unknown { key, value } => Some((key, value)),
            Item::Known { .. } => None,
        }
    }
}

/// A type that can be case-insensitively hashed.
pub trait HashCaseless {
    fn hash_caseless(&self, state: &mut impl Hasher);
}

impl HashCaseless for std::convert::Infallible {
    fn hash_caseless(&self, _state: &mut impl Hasher) {
        unreachable!()
    }
}

impl HashCaseless for str {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self.bytes() {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl HashCaseless for String {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        self.as_str().hash_caseless(state)
    }
}

impl HashCaseless for [u8] {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl<const N: usize> HashCaseless for [u8; N] {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl HashCaseless for Escaped<'_> {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for (_, byte) in self.iter_offsets() {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl<T: ?Sized + HashCaseless> HashCaseless for &T {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        (*self).hash_caseless(state)
    }
}

impl<T: ?Sized + HashCaseless> HashCaseless for &mut T {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        (**self).hash_caseless(state)
    }
}

impl<T: ?Sized + HashCaseless> HashCaseless for Box<T> {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        self.as_ref().hash_caseless(state)
    }
}
