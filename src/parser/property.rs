//! Parsers for properties.

use iri_string::types::UriStr;
use winnow::{
    ModalResult, Parser,
    ascii::Caseless,
    combinator::{alt, fail, preceded, repeat},
    stream::Stream,
    token::none_of,
};

use crate::{
    model::{
        css::Css3Color,
        primitive::{
            AlarmAction, AttachValue, ClassValue, DateTime, DateTimeOrDate,
            Duration, Geo, ImageData, Language, Method, Period, RDate, Status,
            Transparency, Uid, Utc, UtcOffset,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams,
            ImageParams, LangParams, OrganizerParams, RDateParams,
            RecurrenceIdParams, RelTypeParams, TextParamsRef, TriggerParams,
        },
    },
    parser::{
        parameter::{Param, parameter},
        primitive::iana_token,
    },
};

/// The type of "unbounded" unsigned integers.
type UInt = usize;

// NOTE: the IANA iCalendar property registry lists several registered properties
// from RFC 6321 §4.2, RFC 7808 §7, RFC 7953 §3.2, RFC 9073 §6, and RFC 9253 § 8
// that have not been included here (they would fall under the Other catch-all).
// perhaps they should be included as static variants at some later point?
// registry: (https://www.iana.org/assignments/icalendar/icalendar.xhtml#properties)

pub enum Prop<'a> {
    // CALENDAR PROPERTIES
    CalScale,
    Method(Method),
    ProdId(&'a str),
    Version,
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(AttachValue<&'a UriStr>, AttachParams),
    Categories(Box<[&'a str]>, LangParams<&'a str>),
    Class(ClassValue<&'a str>),
    Comment(&'a str, TextParamsRef<'a>),
    Description(&'a str, TextParamsRef<'a>),
    Geo(Geo),
    Location(&'a str, TextParamsRef<'a>),
    PercentComplete(u8), // 0..=100
    Priority(u8),        // 0..=9
    Resources(Box<[&'a str]>, TextParamsRef<'a>),
    Status(Status),
    Summary(&'a str, TextParamsRef<'a>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(DateTime),
    DtEnd(DateTimeOrDate, DtParams<&'a str>),
    DtDue(DateTimeOrDate, DtParams<&'a str>),
    DtStart(DateTimeOrDate, DtParams<&'a str>),
    Duration(Duration),
    FreeBusy(Box<[Period]>, FBTypeParams<&'a str>),
    Transparency(Transparency),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(&'a str),
    TzName(&'a str, LangParams<&'a str>),
    TzOffsetFrom(UtcOffset),
    TzOffsetTo(UtcOffset),
    TzUrl(&'a UriStr),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(&'a UriStr, AttendeeParams<&'a str, &'a UriStr>),
    Contact(&'a str, TextParamsRef<'a>),
    Organizer(&'a UriStr, OrganizerParams<&'a str, &'a UriStr>),
    RecurrenceId(DateTimeOrDate, RecurrenceIdParams<&'a str>),
    RelatedTo(&'a str, RelTypeParams<&'a str>),
    Url(&'a UriStr),
    Uid(Uid<&'a str>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(DateTimeOrDate, DtParams<&'a str>),
    RDate(RDate, RDateParams<&'a str>),
    // TODO: finish recurrence rule model
    RRule(()),
    // ALARM COMPONENT PROPERTIES
    Action(AlarmAction<&'a str>),
    Repeat(UInt),
    TriggerRelative(Duration, TriggerParams),
    TriggerAbsolute(DateTime<Utc>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(DateTime),
    DtStamp(DateTime),
    LastModified(DateTime),
    Sequence(UInt),

    // TODO: distinguish between X-names and IANA-registered properties, and
    // give them a Value enum to correctly track the types of their values

    // MISCELLANEOUS COMPONENT PROPERTIES
    // TODO: the value of this property has a more precise grammar (page 143)
    RequestStatus(&'a str, LangParams<&'a str>),
    Other {
        name: &'a str,
        lanugage: Option<Language<&'a str>>,
        value: &'a str,
    },

    // RFC 7986 PROPERTIES
    Name(&'a str, TextParamsRef<'a>),
    RefreshInterval(Duration), // NOTE: the "VALUE=DURATION" param is REQUIRED
    Source(&'a UriStr),
    Color(Css3Color),
    Image(ImageData<&'a UriStr>, ImageParams<&'a str, &'a UriStr>),
    Conference(&'a UriStr, ConfParams<&'a str>),
}

/// A textual property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property<'a> {
    /// The name of the property.
    pub name: PropName<'a>,
    /// The parameters of the property.
    pub params: Box<[Param<'a>]>,
    /// The raw textual value of the property, which cannot include control
    /// characters (U+0000 through U+001F and U+007F).
    pub value: &'a str,
}

/// Parses a [`Property`].
///
/// # Examples
///
/// ```
/// use calico::parser::property::property;
/// use winnow::Parser;
///
/// assert!(property.parse_peek("TZNAME:EST").is_ok());
/// assert!(property.parse_peek("ATTENDEE;RSVP=TRUE;ROLE=REQ:foo").is_ok());
/// ```
pub fn property<'i>(input: &mut &'i str) -> ModalResult<Property<'i>> {
    /// Parses the value string of a property line.
    fn value<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        repeat::<_, _, (), _, _>(0.., none_of((..' ', '\u{007F}')))
            .take()
            .parse_next(input)
    }

    (
        property_name,
        repeat::<_, _, Vec<_>, _, _>(0.., preceded(';', parameter)),
        ':',
        value,
    )
        .map(|(name, params, _, value)| Property {
            name,
            params: params.into_boxed_slice(),
            value,
        })
        .parse_next(input)
}

/// A property name, which may be statically known from RFC 5545 or RFC 7986, or
/// otherwise may be some arbitrary [`iana_token`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropName<'a> {
    Rfc5545(Rfc5545PropName),
    Rfc7986(Rfc7986PropName),
    Other(&'a str),
}

/// Parses a [`PropName`].
///
/// # Examples
///
/// ```
/// use calico::parser::property::{
///     property_name,
///     PropName,
///     Rfc5545PropName
/// };
/// use winnow::Parser;
///
/// assert_eq!(
///     property_name.parse_peek("ACTION").unwrap().1,
///     PropName::Rfc5545(Rfc5545PropName::Action),
/// );
///
/// assert_eq!(
///     property_name.parse_peek("cOnFeReNcE"),
///     property_name.parse_peek("CONFERENCE"),
/// );
/// ```
pub fn property_name<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
    // NOTE: this internal implementation is a little gross, and arguably i
    // could do better with something like aho-corasick. maybe look at how
    // logos does token parser generation?

    fn other<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        iana_token.map(PropName::Other).parse_next(input)
    }

    fn a_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("ATTENDEE")
                .value(PropName::Rfc5545(Rfc5545PropName::Attendee)),
            Caseless("ATTACH")
                .value(PropName::Rfc5545(Rfc5545PropName::Attachment)),
            Caseless("ACTION")
                .value(PropName::Rfc5545(Rfc5545PropName::Action)),
            other,
        ))
        .parse_next(input)
    }

    fn c_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("CONFERENCE")
                .value(PropName::Rfc7986(Rfc7986PropName::Conference)),
            Caseless("CATEGORIES")
                .value(PropName::Rfc5545(Rfc5545PropName::Categories)),
            Caseless("COMPLETED")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted)),
            Caseless("CALSCALE")
                .value(PropName::Rfc5545(Rfc5545PropName::CalendarScale)),
            Caseless("CONTACT")
                .value(PropName::Rfc5545(Rfc5545PropName::Contact)),
            Caseless("CREATED")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeCreated)),
            Caseless("COMMENT")
                .value(PropName::Rfc5545(Rfc5545PropName::Comment)),
            Caseless("COLOR").value(PropName::Rfc7986(Rfc7986PropName::Color)),
            Caseless("CLASS")
                .value(PropName::Rfc5545(Rfc5545PropName::Classification)),
            other,
        ))
        .parse_next(input)
    }

    fn d_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("DESCRIPTION")
                .value(PropName::Rfc5545(Rfc5545PropName::Description)),
            Caseless("DTSTART")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeStart)),
            Caseless("DTSTAMP")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeStamp)),
            Caseless("DTEND")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeEnd)),
            Caseless("DURATION")
                .value(PropName::Rfc5545(Rfc5545PropName::Duration)),
            Caseless("DUE")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeDue)),
            other,
        ))
        .parse_next(input)
    }

    fn e_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("EXDATE")
                .value(PropName::Rfc5545(Rfc5545PropName::ExceptionDateTimes)),
            other,
        ))
        .parse_next(input)
    }

    fn f_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("FREEBUSY")
                .value(PropName::Rfc5545(Rfc5545PropName::FreeBusyTime)),
            other,
        ))
        .parse_next(input)
    }

    fn g_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("GEO")
                .value(PropName::Rfc5545(Rfc5545PropName::GeographicPosition)),
            other,
        ))
        .parse_next(input)
    }

    fn i_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("IMAGE").value(PropName::Rfc7986(Rfc7986PropName::Image)),
            other,
        ))
        .parse_next(input)
    }

    fn l_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("LAST-MODIFIED")
                .value(PropName::Rfc5545(Rfc5545PropName::LastModified)),
            Caseless("LOCATION")
                .value(PropName::Rfc5545(Rfc5545PropName::Location)),
            other,
        ))
        .parse_next(input)
    }

    fn m_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("METHOD")
                .value(PropName::Rfc5545(Rfc5545PropName::Method)),
            other,
        ))
        .parse_next(input)
    }

    fn n_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("NAME").value(PropName::Rfc7986(Rfc7986PropName::Name)),
            other,
        ))
        .parse_next(input)
    }

    fn o_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("ORGANIZER")
                .value(PropName::Rfc5545(Rfc5545PropName::Organizer)),
            other,
        ))
        .parse_next(input)
    }

    fn p_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("PERCENT-COMPLETE")
                .value(PropName::Rfc5545(Rfc5545PropName::PercentComplete)),
            Caseless("PRIORITY")
                .value(PropName::Rfc5545(Rfc5545PropName::Priority)),
            Caseless("PRODID")
                .value(PropName::Rfc5545(Rfc5545PropName::ProductIdentifier)),
            other,
        ))
        .parse_next(input)
    }

    fn r_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("REFRESH-INTERVAL")
                .value(PropName::Rfc7986(Rfc7986PropName::RefreshInterval)),
            Caseless("REQUEST-STATUS")
                .value(PropName::Rfc5545(Rfc5545PropName::RequestStatus)),
            Caseless("RECURRENCE-ID")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceId)),
            Caseless("RELATED-TO")
                .value(PropName::Rfc5545(Rfc5545PropName::RelatedTo)),
            Caseless("RESOURCES")
                .value(PropName::Rfc5545(Rfc5545PropName::Resources)),
            Caseless("RDATE")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes)),
            Caseless("RRULE")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceRule)),
            Caseless("REPEAT")
                .value(PropName::Rfc5545(Rfc5545PropName::RepeatCount)),
            other,
        ))
        .parse_next(input)
    }

    fn s_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("SEQUENCE")
                .value(PropName::Rfc5545(Rfc5545PropName::SequenceNumber)),
            Caseless("SUMMARY")
                .value(PropName::Rfc5545(Rfc5545PropName::Summary)),
            Caseless("STATUS")
                .value(PropName::Rfc5545(Rfc5545PropName::Status)),
            Caseless("SOURCE")
                .value(PropName::Rfc7986(Rfc7986PropName::Source)),
            other,
        ))
        .parse_next(input)
    }

    fn t_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("TZOFFSETFROM")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom)),
            Caseless("TZOFFSETTO")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo)),
            Caseless("TZNAME")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneName)),
            Caseless("TZURL")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl)),
            Caseless("TZID")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier)),
            Caseless("TRIGGER")
                .value(PropName::Rfc5545(Rfc5545PropName::Trigger)),
            Caseless("TRANSP")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeTransparency)),
            other,
        ))
        .parse_next(input)
    }

    fn u_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("URL").value(PropName::Rfc5545(
                Rfc5545PropName::UniformResourceLocator,
            )),
            Caseless("UID")
                .value(PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier)),
            other,
        ))
        .parse_next(input)
    }

    fn v_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("VERSION")
                .value(PropName::Rfc5545(Rfc5545PropName::Version)),
            other,
        ))
        .parse_next(input)
    }

    match input.peek_token() {
        Some('A' | 'a') => a_names.parse_next(input),
        Some('C' | 'c') => c_names.parse_next(input),
        Some('D' | 'd') => d_names.parse_next(input),
        Some('E' | 'e') => e_names.parse_next(input),
        Some('F' | 'f') => f_names.parse_next(input),
        Some('G' | 'g') => g_names.parse_next(input),
        Some('I' | 'i') => i_names.parse_next(input),
        Some('L' | 'l') => l_names.parse_next(input),
        Some('M' | 'm') => m_names.parse_next(input),
        Some('N' | 'n') => n_names.parse_next(input),
        Some('O' | 'o') => o_names.parse_next(input),
        Some('P' | 'p') => p_names.parse_next(input),
        Some('R' | 'r') => r_names.parse_next(input),
        Some('S' | 's') => s_names.parse_next(input),
        Some('T' | 't') => t_names.parse_next(input),
        Some('U' | 'u') => u_names.parse_next(input),
        Some('V' | 'v') => v_names.parse_next(input),
        Some(_) => other.parse_next(input),
        None => fail.parse_next(input),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc5545PropName {
    // CALENDAR PROPERTIES (RFC 5545 §3.7)
    /// RFC 5545 §3.7.1 (CALSCALE)
    CalendarScale,
    /// RFC 5545 §3.7.2 (METHOD)
    Method,
    /// RFC 5545 §3.7.3 (PRODID)
    ProductIdentifier,
    /// RFC 5545 §3.7.4 (VERSION)
    Version,

    // DESCRIPTIVE PROPERTIES (RFC 5545 §3.8.1)
    /// RFC 5545 §3.8.1.1 (ATTACH)
    Attachment,
    /// RFC 5545 §3.8.1.2 (CATEGORIES)
    Categories,
    /// RFC 5545 §3.8.1.3 (CLASS)
    Classification,
    /// RFC 5545 §3.8.1.4 (COMMENT)
    Comment,
    /// RFC 5545 §3.8.1.5 (DESCRIPTION)
    Description,
    /// RFC 5545 §3.8.1.6 (GEO)
    GeographicPosition,
    /// RFC 5545 §3.8.1.7 (LOCATION)
    Location,
    /// RFC 5545 §3.8.1.8 (PERCENT-COMPLETE)
    PercentComplete,
    /// RFC 5545 §3.8.1.9 (PRIORITY)
    Priority,
    /// RFC 5545 §3.8.1.10 (RESOURCES)
    Resources,
    /// RFC 5545 §3.8.1.11 (STATUS)
    Status,
    /// RFC 5545 §3.8.1.12 (SUMMARY)
    Summary,

    // DATE AND TIME PROPERTIES (RFC 5545 §3.8.2)
    /// RFC 5545 §3.8.2.1 (COMPLETED)
    DateTimeCompleted,
    /// RFC 5545 §3.8.2.2 (DTEND)
    DateTimeEnd,
    /// RFC 5545 §3.8.2.3 (DUE)
    DateTimeDue,
    /// RFC 5545 §3.8.2.4 (DTSTART)
    DateTimeStart,
    /// RFC 5545 §3.8.2.5 (DURATION)
    Duration,
    /// RFC 5545 §3.8.2.6 (FREEBUSY)
    FreeBusyTime,
    /// RFC 5545 §3.8.2.7 (TRANSP)
    TimeTransparency,

    // TIME ZONE PROPERTIES (RFC 5545 §3.8.3)
    /// RFC 5545 §3.8.3.1 (TZID)
    TimeZoneIdentifier,
    /// RFC 5545 §3.8.3.2 (TZNAME)
    TimeZoneName,
    /// RFC 5545 §3.8.3.3 (TZOFFSETFROM)
    TimeZoneOffsetFrom,
    /// RFC 5545 §3.8.3.4 (TZOFFSETTO)
    TimeZoneOffsetTo,
    /// RFC 5545 §3.8.3.5 (TZURL)
    TimeZoneUrl,

    // RELATIONSHIP PROPERTIES (RFC 5545 §3.8.4)
    /// RFC 5545 §3.8.4.1 (ATTENDEE)
    Attendee,
    /// RFC 5545 §3.8.4.2 (CONTACT)
    Contact,
    /// RFC 5545 §3.8.4.3 (ORGANIZER)
    Organizer,
    /// RFC 5545 §3.8.4.4 (RECURRENCE-ID)
    RecurrenceId,
    /// RFC 5545 §3.8.4.5 (RELATED-TO)
    RelatedTo,
    /// RFC 5545 §3.8.4.6 (URL)
    UniformResourceLocator,
    /// RFC 5545 §3.8.4.7 (UID)
    UniqueIdentifier,

    // RECURRENCE PROPERTIES (RFC 5545 §3.8.5)
    /// RFC 5545 §3.8.5.1 (EXDATE)
    ExceptionDateTimes,
    /// RFC 5545 §3.8.5.2 (RDATE)
    RecurrenceDateTimes,
    /// RFC 5545 §3.8.5.3 (RRULE)
    RecurrenceRule,

    // ALARM PROPERTIES (RFC 5545 §3.8.6)
    /// RFC 5545 §3.8.6.1 (ACTION)
    Action,
    /// RFC 5545 §3.8.6.2 (REPEAT)
    RepeatCount,
    /// RFC 5545 §3.8.6.3 (TRIGGER)
    Trigger,

    // CHANGE MANAGEMENT PROPERTIES (RFC 5545 §3.8.7)
    /// RFC 5545 §3.8.7.1 (CREATED)
    DateTimeCreated,
    /// RFC 5545 §3.8.7.2 (DTSTAMP)
    DateTimeStamp,
    /// RFC 5545 §3.8.7.3 (LAST-MODIFIED)
    LastModified,
    /// RFC 5545 §3.8.7.4 (SEQUENCE)
    SequenceNumber,

    // MISCELLANEOUS PROPERTIES (RFC 5545 §3.8.8)
    /// RFC 5545 §3.8.8.3 (REQUEST-STATUS)
    RequestStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc7986PropName {
    /// RFC 7986 §5.1 (NAME)
    Name,
    /// RFC 7986 §5.7 (REFRESH-INTERVAL)
    RefreshInterval,
    /// RFC 7986 §5.8 (SOURCE)
    Source,
    /// RFC 7986 §5.9 (COLOR)
    Color,
    /// RFC 7986 §5.10 (IMAGE)
    Image,
    /// RFC 7986 §5.11 (CONFERENCE)
    Conference,
}

#[cfg(test)]
mod tests {
    use super::*;
    use winnow::Parser;

    // PROPERTY NAME TESTS

    /// Asserts that the inputs are equal under [`property_name`].
    fn assert_prop_name_eq<'i>(input: &'i str, expected: PropName<'i>) {
        let mut input_ref = input;
        let result = property_name.parse_next(&mut input_ref);
        assert!(result.is_ok(), "Failed to parse '{}': {:?}", input, result);
        assert_eq!(result.unwrap(), expected);
        assert!(
            input_ref.is_empty(),
            "Input not fully consumed: '{}'",
            input_ref
        );
    }

    // Helper function to test parsing failures
    fn assert_prop_name_parse_failure(input: &str) {
        let mut input_ref = input;
        let result = property_name.parse_next(&mut input_ref);
        assert!(result.is_err(), "Expected parsing to fail for '{}'", input);
    }

    #[test]
    fn rfc5545_calendar_property_names() {
        assert_prop_name_eq(
            "CALSCALE",
            PropName::Rfc5545(Rfc5545PropName::CalendarScale),
        );
        assert_prop_name_eq(
            "METHOD",
            PropName::Rfc5545(Rfc5545PropName::Method),
        );
        assert_prop_name_eq(
            "PRODID",
            PropName::Rfc5545(Rfc5545PropName::ProductIdentifier),
        );
        assert_prop_name_eq(
            "VERSION",
            PropName::Rfc5545(Rfc5545PropName::Version),
        );
    }

    #[test]
    fn rfc5545_descriptive_property_names() {
        assert_prop_name_eq(
            "ATTACH",
            PropName::Rfc5545(Rfc5545PropName::Attachment),
        );
        assert_prop_name_eq(
            "CATEGORIES",
            PropName::Rfc5545(Rfc5545PropName::Categories),
        );
        assert_prop_name_eq(
            "CLASS",
            PropName::Rfc5545(Rfc5545PropName::Classification),
        );
        assert_prop_name_eq(
            "COMMENT",
            PropName::Rfc5545(Rfc5545PropName::Comment),
        );
        assert_prop_name_eq(
            "DESCRIPTION",
            PropName::Rfc5545(Rfc5545PropName::Description),
        );
        assert_prop_name_eq(
            "GEO",
            PropName::Rfc5545(Rfc5545PropName::GeographicPosition),
        );
        assert_prop_name_eq(
            "LOCATION",
            PropName::Rfc5545(Rfc5545PropName::Location),
        );
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
        assert_prop_name_eq(
            "PRIORITY",
            PropName::Rfc5545(Rfc5545PropName::Priority),
        );
        assert_prop_name_eq(
            "RESOURCES",
            PropName::Rfc5545(Rfc5545PropName::Resources),
        );
        assert_prop_name_eq(
            "STATUS",
            PropName::Rfc5545(Rfc5545PropName::Status),
        );
        assert_prop_name_eq(
            "SUMMARY",
            PropName::Rfc5545(Rfc5545PropName::Summary),
        );
    }

    #[test]
    fn rfc5545_datetime_property_names() {
        assert_prop_name_eq(
            "COMPLETED",
            PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted),
        );
        assert_prop_name_eq(
            "DTEND",
            PropName::Rfc5545(Rfc5545PropName::DateTimeEnd),
        );
        assert_prop_name_eq(
            "DUE",
            PropName::Rfc5545(Rfc5545PropName::DateTimeDue),
        );
        assert_prop_name_eq(
            "DTSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DURATION",
            PropName::Rfc5545(Rfc5545PropName::Duration),
        );
        assert_prop_name_eq(
            "FREEBUSY",
            PropName::Rfc5545(Rfc5545PropName::FreeBusyTime),
        );
        assert_prop_name_eq(
            "TRANSP",
            PropName::Rfc5545(Rfc5545PropName::TimeTransparency),
        );
        assert_prop_name_eq(
            "DTSTAMP",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStamp),
        );
    }

    #[test]
    fn rfc5545_timezone_property_names() {
        assert_prop_name_eq(
            "TZID",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier),
        );
        assert_prop_name_eq(
            "TZNAME",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneName),
        );
        assert_prop_name_eq(
            "TZOFFSETFROM",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
        );
        assert_prop_name_eq(
            "TZOFFSETTO",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
        );
        assert_prop_name_eq(
            "TZURL",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl),
        );
    }

    #[test]
    fn rfc5545_relationship_property_names() {
        assert_prop_name_eq(
            "ATTENDEE",
            PropName::Rfc5545(Rfc5545PropName::Attendee),
        );
        assert_prop_name_eq(
            "CONTACT",
            PropName::Rfc5545(Rfc5545PropName::Contact),
        );
        assert_prop_name_eq(
            "ORGANIZER",
            PropName::Rfc5545(Rfc5545PropName::Organizer),
        );
        assert_prop_name_eq(
            "RECURRENCE-ID",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        );
        assert_prop_name_eq(
            "RELATED-TO",
            PropName::Rfc5545(Rfc5545PropName::RelatedTo),
        );
        assert_prop_name_eq(
            "URL",
            PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator),
        );
        assert_prop_name_eq(
            "UID",
            PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier),
        );
    }

    #[test]
    fn rfc5545_recurrence_property_names() {
        assert_prop_name_eq(
            "EXDATE",
            PropName::Rfc5545(Rfc5545PropName::ExceptionDateTimes),
        );
        assert_prop_name_eq(
            "RDATE",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes),
        );
        assert_prop_name_eq(
            "RRULE",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceRule),
        );
    }

    #[test]
    fn rfc5545_alarm_property_names() {
        assert_prop_name_eq(
            "ACTION",
            PropName::Rfc5545(Rfc5545PropName::Action),
        );
        assert_prop_name_eq(
            "REPEAT",
            PropName::Rfc5545(Rfc5545PropName::RepeatCount),
        );
        assert_prop_name_eq(
            "TRIGGER",
            PropName::Rfc5545(Rfc5545PropName::Trigger),
        );
    }

    #[test]
    fn rfc5545_change_management_property_names() {
        assert_prop_name_eq(
            "CREATED",
            PropName::Rfc5545(Rfc5545PropName::DateTimeCreated),
        );
        assert_prop_name_eq(
            "LAST-MODIFIED",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "SEQUENCE",
            PropName::Rfc5545(Rfc5545PropName::SequenceNumber),
        );
    }

    #[test]
    fn rfc5545_miscellaneous_property_names() {
        assert_prop_name_eq(
            "REQUEST-STATUS",
            PropName::Rfc5545(Rfc5545PropName::RequestStatus),
        );
    }

    #[test]
    fn rfc7986_property_names() {
        assert_prop_name_eq("NAME", PropName::Rfc7986(Rfc7986PropName::Name));
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );
        assert_prop_name_eq(
            "SOURCE",
            PropName::Rfc7986(Rfc7986PropName::Source),
        );
        assert_prop_name_eq("COLOR", PropName::Rfc7986(Rfc7986PropName::Color));
        assert_prop_name_eq("IMAGE", PropName::Rfc7986(Rfc7986PropName::Image));
        assert_prop_name_eq(
            "CONFERENCE",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
    }

    #[test]
    fn property_name_case_insensitivity() {
        assert_prop_name_eq(
            "dtstart",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DTSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DtStArT",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "dtSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );

        assert_prop_name_eq(
            "conference",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
        assert_prop_name_eq(
            "Conference",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
        assert_prop_name_eq(
            "CONFERENCE",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
    }

    #[test]
    fn iana_property_names() {
        assert_prop_name_eq("UNKNOWN-PROP", PropName::Other("UNKNOWN-PROP"));
        assert_prop_name_eq("CUSTOM", PropName::Other("CUSTOM"));
        assert_prop_name_eq(
            "VENDOR-SPECIFIC",
            PropName::Other("VENDOR-SPECIFIC"),
        );
        assert_prop_name_eq("NEW-FEATURE", PropName::Other("NEW-FEATURE"));
    }

    #[test]
    fn x_property_names() {
        assert_prop_name_eq("X-CUSTOM", PropName::Other("X-CUSTOM"));
        assert_prop_name_eq("X-VENDOR-PROP", PropName::Other("X-VENDOR-PROP"));
        assert_prop_name_eq("x-custom", PropName::Other("x-custom"));
    }

    #[test]
    fn property_name_longest_match_precedence() {
        // Ensure longer properties are matched correctly when they share prefixes
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );
        assert_prop_name_eq(
            "REQUEST-STATUS",
            PropName::Rfc5545(Rfc5545PropName::RequestStatus),
        );
        assert_prop_name_eq(
            "RECURRENCE-ID",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        );

        // Make sure we don't match shorter prefixes
        assert_prop_name_eq("REFRESH", PropName::Other("REFRESH"));
        assert_prop_name_eq("REQUEST", PropName::Other("REQUEST"));
        assert_prop_name_eq("RECURRENCE", PropName::Other("RECURRENCE"));
    }

    #[test]
    fn property_name_edge_cases() {
        // Empty input
        assert_prop_name_parse_failure("");

        // Single characters
        assert_prop_name_eq("A", PropName::Other("A"));
        assert_prop_name_eq("Z", PropName::Other("Z"));

        // Properties with hyphens
        assert_prop_name_eq(
            "LAST-MODIFIED",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );

        // Mixed case with hyphens
        assert_prop_name_eq(
            "last-modified",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "Percent-Complete",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
    }
}
