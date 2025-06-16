//! The [`Lines`] iterator.

use std::io::BufRead;

/// An error produced when trying to read an iCalendar line.
#[derive(Debug)]
pub enum LineError {
    /// A missing CRLF line ending.
    MissingCrlf,
    /// A bare CR or LF character.
    InvalidLineEnding,
    /// Unexpected end-of-file.
    UnexpectedEof,
    /// An [IO error].
    ///
    /// [IO error]: std::io::Error
    Io(std::io::Error),
}

impl From<std::io::Error> for LineError {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
}

/// An iterator yielding iCalendar lines, with CRLF continuations escaped and
/// CRLF line endings removed.
#[derive(Debug, Clone, Copy)]
pub struct Lines<B> {
    reader: B,
}

const DEFAULT_LINE_CAPACITY: usize = 20;

impl<B: BufRead> Iterator for Lines<B> {
    type Item = Result<String, LineError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut line = String::with_capacity(DEFAULT_LINE_CAPACITY);
        let mut first_segment = true;

        loop {
            let initial_len = line.len();

            // read up to the next newline
            match self.reader.read_line(&mut line) {
                // reached EOF
                Ok(0) => {
                    return if first_segment {
                        None
                    } else {
                        Some(Err(LineError::UnexpectedEof))
                    };
                }
                Ok(_) => {
                    first_segment = false;
                    let new_segment = &line[initial_len..];

                    if new_segment.ends_with("\r\n") {
                        line.truncate(line.len() - 2);
                    } else if new_segment.ends_with('\n') {
                        // illegal newline
                        return Some(Err(LineError::InvalidLineEnding));
                    } else {
                        // EOF within line
                        return Some(Err(LineError::MissingCrlf));
                    }

                    match self.peek_and_consume_continuation() {
                        Ok(true) => continue,
                        Ok(false) => break,
                        Err(err) => return Some(Err(err)),
                    }
                }
                Err(err) => return Some(Err(LineError::Io(err))),
            }
        }

        line.shrink_to_fit();
        Some(Ok(line))
    }
}

impl<B: BufRead> Lines<B> {
    pub fn new(reader: B) -> Self {
        Self { reader }
    }

    /// Checks for a continuation whitespace character (space or htab) and
    /// consumes it and returns `Some(true)` if it exists.
    fn peek_and_consume_continuation(&mut self) -> Result<bool, LineError> {
        let buf = self.reader.fill_buf()?;
        if !buf.is_empty() && (buf[0] == b' ' || buf[0] == b'\t') {
            self.reader.consume(1);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    const EXAMPLE1: &str =
        include_str!("../../examples/rfc5545-section-4-example-1.ics");

    const EXAMPLE2: &str =
        include_str!("../../examples/rfc5545-section-4-example-2.ics");

    #[test]
    fn rfc5545_section_4_example_1() {
        let reader = Cursor::new(EXAMPLE1);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        let expected_lines = vec![
            "BEGIN:VCALENDAR",
            "PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN",
            "VERSION:2.0",
            "BEGIN:VEVENT",
            "DTSTAMP:19960704T120000Z",
            "UID:uid1@example.com",
            "ORGANIZER:mailto:jsmith@example.com",
            "DTSTART:19960918T143000Z",
            "DTEND:19960920T220000Z",
            "STATUS:CONFIRMED",
            "CATEGORIES:CONFERENCE",
            "SUMMARY:Networld+Interop Conference",
            "DESCRIPTION:Networld+Interop Conference and Exhibit\\nAtlanta World Congress Center\\nAtlanta\\, Georgia",
            "END:VEVENT",
            "END:VCALENDAR",
        ];

        assert_eq!(lines.len(), expected_lines.len());
        for (got, expected) in lines.iter().zip(expected_lines) {
            assert_eq!(got, expected);
        }
    }

    #[test]
    fn rfc5545_section_4_example_2() {
        let reader = Cursor::new(EXAMPLE2);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        let expected_lines = vec![
            "BEGIN:VCALENDAR",
            "PRODID:-//RDU Software//NONSGML HandCal//EN",
            "VERSION:2.0",
            "BEGIN:VTIMEZONE",
            "TZID:America/New_York",
            "BEGIN:STANDARD",
            "DTSTART:19981025T020000",
            "TZOFFSETFROM:-0400",
            "TZOFFSETTO:-0500",
            "TZNAME:EST",
            "END:STANDARD",
            "BEGIN:DAYLIGHT",
            "DTSTART:19990404T020000",
            "TZOFFSETFROM:-0500",
            "TZOFFSETTO:-0400",
            "TZNAME:EDT",
            "END:DAYLIGHT",
            "END:VTIMEZONE",
            "BEGIN:VEVENT",
            "DTSTAMP:19980309T231000Z",
            "UID:guid-1.example.com",
            "ORGANIZER:mailto:mrbig@example.com",
            "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT;CUTYPE=GROUP:mailto:employee-A@example.com",
            "DESCRIPTION:Project XYZ Review Meeting",
            "CATEGORIES:MEETING",
            "CLASS:PUBLIC",
            "CREATED:19980309T130000Z",
            "SUMMARY:XYZ Project Review",
            "DTSTART;TZID=America/New_York:19980312T083000",
            "DTEND;TZID=America/New_York:19980312T093000",
            "LOCATION:1CP Conference Room 4350",
            "END:VEVENT",
            "END:VCALENDAR",
        ];

        assert_eq!(lines.len(), expected_lines.len());
        for (got, expected) in lines.iter().zip(expected_lines) {
            assert_eq!(got, expected);
        }
    }

    #[test]
    fn basic_line_splitting() {
        let input = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nEND:VCALENDAR\r\n";
        let reader = Cursor::new(input);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        assert_eq!(
            lines,
            vec!["BEGIN:VCALENDAR", "VERSION:2.0", "END:VCALENDAR",]
        );
    }

    #[test]
    fn basic_line_folding() {
        let input = "DESCRIPTION:This is a long description\r\n  that is folded\r\n  across multiple lines\r\n";
        let reader = Cursor::new(input);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        assert_eq!(
            lines,
            vec![
                "DESCRIPTION:This is a long description that is folded across multiple lines"
            ]
        );
    }

    #[test]
    fn tab_folding() {
        let input = "SUMMARY:Meeting\r\n\twith details\r\n";
        let reader = Cursor::new(input);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        assert_eq!(lines, vec!["SUMMARY:Meetingwith details"]);
    }

    #[test]
    fn complex_folding() {
        let input = concat!(
            "DESCRIPTION:This is a very long line\r\n",
            "  that spans\r\n",
            "\t multiple continuation\r\n",
            "  lines with mixed whitespace\r\n",
            "SUMMARY:Next property\r\n"
        );

        let reader = Cursor::new(input);
        let lines: Result<Vec<_>, _> = Lines { reader }.collect();
        let lines = lines.unwrap();

        assert_eq!(
            lines,
            vec![
                "DESCRIPTION:This is a very long line that spans multiple continuation lines with mixed whitespace",
                "SUMMARY:Next property"
            ]
        );
    }

    #[test]
    fn empty_input() {
        let input = "";
        let reader = Cursor::new(input);
        let lines: Vec<_> = Lines { reader }.collect();
        assert!(lines.is_empty());
    }

    #[test]
    fn invalid_newline() {
        let input = "BEGIN:VCALENDAR\r\nVERSION:2.0\nEND:VCALENDAR\r\n";
        let reader = Cursor::new(input);
        let results: Vec<_> = Lines { reader }.collect();

        assert!(
            results
                .iter()
                .any(|r| matches!(r, Err(LineError::InvalidLineEnding)))
        );
    }

    #[test]
    fn missing_final_crlf() {
        let input = "BEGIN:VCALENDAR\r\nVERSION:2.0";
        let reader = Cursor::new(input);
        let results: Vec<_> = Lines { reader }.collect();

        assert_eq!(results.len(), 2);
        assert!(results[0].as_ref().is_ok_and(|s| s == "BEGIN:VCALENDAR"));
        assert!(matches!(results[1], Err(LineError::MissingCrlf)));
    }
}
