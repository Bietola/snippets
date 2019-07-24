trait State: std::fmt::Debug {
    fn request_review(self: Box<Self>) -> Box<dyn State>; 

    fn approve(self: Box<Self>) -> Box<dyn State>;
}

#[derive(Debug)]
struct Draft {}
impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        Box::new(Review {})
    }
    
    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
}

#[derive(Debug)]
struct Review {}
impl State for Review {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
    fn approve(self: Box<Self>) -> Box<dyn State> {
        Box::new(Published {})
    }
}

#[derive(Debug)]
struct Published {} 
impl State for Published {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
}

#[derive(Debug)]
struct Post {
    state: Option<Box<dyn State>>,
    contents: String,
}

impl Post {
    fn new() -> Self {
        Post {
            contents: String::from(""),
            state: Some(Box::new(Draft {}))
        }
    }

    fn add_text(&mut self, text: &str) {
        self.contents.push_str(text);
    }

    fn request_review(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.request_review());
        }
    }

    fn approve(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.approve());
        }
    }
}

fn main() {
    let mut post = Post::new();

    post.add_text("Hello world!");
    println!("{:?}", post);

    post.request_review();
    println!("{:?}", post);

    post.approve();
    println!("{:?}", post);
}
