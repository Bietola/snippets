extern crate itertools;
extern crate chrono;
extern crate time;

use chrono::*;
use itertools::*;
use time::Duration;

/**********************************************************/
/* Iterator used for iterating over ranges of naive dates */
/**********************************************************/
struct DateItr {
    date: NaiveDate,
}

impl DateItr {
    fn new(date: NaiveDate) -> Self {
        Self { date }
    }

    fn from_ymd(year: i32, month: u32, day: u32) -> Self {
        Self::new(NaiveDate::from_ymd(year, month, day))
    }
}

impl Iterator for DateItr {
    type Item = NaiveDate;

    fn next(&mut self) -> Option<Self::Item> {
        self.date += Duration::days(1);

        Some(self.date)
    }
}

/*********************/
/* Support functions */
/*********************/
fn dates_in_year(year: i32) -> impl Iterator<Item = NaiveDate> {
    DateItr::from_ymd(year, 1, 1)
        .take_while(move |d| d.year() == year)
}

/********/
/* Main */
/********/
fn main() {
    let itr = DateItr::new(NaiveDate::from_ymd(2015, 1, 1));

    println!("{}", dates_in_year(2015)
             .group_by(|d| d.month())
             .flatten()
             .format("\n"));
}
