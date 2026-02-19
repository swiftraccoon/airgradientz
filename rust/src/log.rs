use std::time::{SystemTime, UNIX_EPOCH};

/// Returns `true` when `y` is a leap year.
const fn is_leap_year(y: i64) -> bool {
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)
}

/// Number of days in `month` (1-based) of year `y`.
const fn days_in_month(y: i64, m: u32) -> u32 {
    match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            if is_leap_year(y) {
                29
            } else {
                28
            }
        }
        _ => 0,
    }
}

/// Format the current UTC time as `[YYYY-MM-DD HH:MM:SS]`.
pub(crate) fn timestamp_prefix() -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    #[allow(clippy::cast_possible_wrap)]
    let mut remaining = secs as i64;

    let sec = remaining % 60;
    remaining /= 60;
    let min = remaining % 60;
    remaining /= 60;
    let hour = remaining % 24;
    let mut days = remaining / 24;

    // Compute year from day count (epoch = 1970-01-01)
    let mut year: i64 = 1970;
    loop {
        let days_in_year = if is_leap_year(year) { 366 } else { 365 };
        if days < days_in_year {
            break;
        }
        days -= days_in_year;
        year += 1;
    }

    // Compute month and day
    let mut month: u32 = 1;
    loop {
        let dim = i64::from(days_in_month(year, month));
        if days < dim {
            break;
        }
        days -= dim;
        month += 1;
    }
    let day = days + 1;

    format!("[{year:04}-{month:02}-{day:02} {hour:02}:{min:02}:{sec:02}]")
}

/// Log a timestamped message to stderr.
///
/// Usage: `log!("[tag] message {}", value);`
macro_rules! log {
    ($($arg:tt)*) => {
        eprintln!("{} {}", $crate::log::timestamp_prefix(), format_args!($($arg)*))
    };
}

pub(crate) use log;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timestamp_format() {
        let ts = timestamp_prefix();
        // Should match [YYYY-MM-DD HH:MM:SS]
        assert_eq!(ts.len(), 21);
        assert_eq!(&ts[0..1], "[");
        assert_eq!(&ts[20..21], "]");
        assert_eq!(&ts[5..6], "-");
        assert_eq!(&ts[8..9], "-");
        assert_eq!(&ts[11..12], " ");
        assert_eq!(&ts[14..15], ":");
        assert_eq!(&ts[17..18], ":");
    }

    #[test]
    fn test_is_leap_year() {
        assert!(is_leap_year(2000));
        assert!(is_leap_year(2024));
        assert!(!is_leap_year(1900));
        assert!(!is_leap_year(2023));
    }

    #[test]
    fn test_days_in_month() {
        assert_eq!(days_in_month(2024, 2), 29);
        assert_eq!(days_in_month(2023, 2), 28);
        assert_eq!(days_in_month(2024, 1), 31);
        assert_eq!(days_in_month(2024, 4), 30);
    }
}
