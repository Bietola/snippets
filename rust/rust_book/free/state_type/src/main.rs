struct Post {
}

impl Post {
    fn new() -> Draft {
        Draft { contents: String::new() }
    }
}

struct Draft {
    contents: String,
}

impl Draft {
    fn request_review(self) -> PendingReview {
        PendingReview { contents: self.contents }
    }

    fn add_text(&mut self, addition: &str) {
        self.contents.push_str(addition);
    }

    fn clear(mut self) -> Self {
        self.contents.clear();
        self
    }
}

struct PendingReview {
    contents: String,
}

impl PendingReview {
    fn approve(mut self) -> Published {
        self.approvals += 1;
        Published { contents: self.contents }
    }

    fn reject(self) -> Draft {
        Draft { contents: self.contents }
    }
}

struct Published {
    contents: String,
}

impl Published {
    fn contents(&self) -> &str {
        &self.contents
    }
}

fn main() {
    let mut p = Post::new();
    p.add_text("My hands hurt...");
    p.add_text(" But that's ok");
    let p = p.request_review();
    let mut p = p.reject().clear();
    p.add_text("I will get better after I rest.");
    p.add_text(" And then I'll be back to implementing all sort of things!");
    let p = p.request_review().approve();
    println!("{}", p.contents());
}
