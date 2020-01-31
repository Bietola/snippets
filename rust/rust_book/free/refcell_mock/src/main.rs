/*************/
/* Messenger */
/*************/
pub trait Messenger {
    fn send<T: AsRef<str>>(&self, msg: T);
}

/***************/
/* BoundCheker */
/***************/
pub struct BoundChecker<'a, T: Messenger> {
    value: u32,
    max: u32,
    messenger: &'a T,
}

impl<'a, T: Messenger> BoundChecker<'a, T> {
    fn new(max: u32, messenger: &'a T) -> Self {
        Self { value: 0, max, messenger }
    }

    fn set_value(&mut self, value: u32) {
        self.value = value;

        let portion_covered = self.value as f64 / self.max as f64;

        if portion_covered < 0.75 {
            self.messenger.send(format!["Value set to {}", self.value]);
        } else if portion_covered < 1.0 {
            self.messenger.send(format!["WARNING: Over the 75% limit... [{}]", self.value]);
        } else {
            self.messenger.send(format![
                "ERROR: Over the imposed limit! [{}, limit: {}]",
                self.value,
                self.max
            ]);
        }
    }
}

fn main() {
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    struct MockMessenger {
        pub messages: RefCell<Vec<String>>,
    }

    impl MockMessenger {
        pub fn new() -> Self {
            Self { messages: RefCell::new(vec![]) }
        }
    }

    impl Messenger for MockMessenger {
        fn send<T: AsRef<str>>(&self, msg: T) {
            self.messages.borrow_mut().push(msg.as_ref().to_string());
        }
    }

    #[test]
    fn over_75() {
        let mock_messenger = MockMessenger::new();

        let mut checker = BoundChecker::new(10, &mock_messenger);
        checker.set_value(9);
        checker.set_value(10);

        assert_eq!(mock_messenger.messages.borrow().len(), 2);
    }
}
