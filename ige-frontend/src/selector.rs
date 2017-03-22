use cgmath::Point2;
use display::DisplayInput;
use std::collections::VecDeque;
use std::slice;

#[derive(Clone, Debug)]
pub struct Selector {
    pub prefix: String,
    pub labels: Vec<Label>,
}

#[derive(Clone, Debug)]
pub struct Label {
    pub point: Point2<f64>,
    pub node_id: u32,
    pub text: String
}

impl Label {
    pub fn change_text(&self, new_text: String) -> Label {
        Label {
            point: self.point,
            node_id: self.node_id,
            text: new_text
        }
    }
}

pub enum SelectionResult {
    Canceled,
    NodeId (u32),
    Continue (Selector)
}

const NUM_CHARS: usize = 9;
const CHARS: [&'static str; NUM_CHARS] = ["a", "s", "d", "f", "g", "h", "j", "k", "l"];

enum PreLabel {
    Unexpanded (String),
    Expanded (String)
}

impl Selector {
    pub fn from_display_input(di: &DisplayInput) -> Selector {
        use self::PreLabel::{Unexpanded, Expanded};
        let selectors = &di.selectors[..];
        let mut queue = VecDeque::new();
        queue.push_back(Expanded("".to_string()));
        for _ in 0..selectors.len() {
            match queue.pop_front().unwrap() {
                Expanded (s) => {
                    let chars = &CHARS;
                    let mut char_iter = chars.iter();
                    queue.push_back(Expanded(s.clone() + char_iter.next().unwrap()));
                    for c in char_iter {
                        queue.push_front(Unexpanded(s.clone() + c));
                    }
                }
                Unexpanded (s) => {
                    queue.push_back(Expanded(s));
                }
            }
        }
        let mut tags = queue.drain(..).filter_map(|pl| {
            match pl {
                Expanded (s) => Some(s),
                Unexpanded (_) => None
            }
        }).collect::<Vec<_>>();
        (&mut tags).sort();
        let labels = tags.drain(..).zip(selectors.iter()).map(|(s, &(p, node_id))| {
            Label { point: p, node_id: node_id, text: s }
        }).collect();
        Selector {
            prefix: String::new(),
            labels: labels
        }
    }


    pub fn add_char(&self, ch: char) -> SelectionResult {
        let new_labels = self.labels.iter().filter_map(|label| {
            let mut chars = label.text.chars();
            if chars.next() == Some(ch) {
                Some (label.change_text(chars.collect()))
            } else {
                None
            }
        }).collect::<Vec<_>>();
        if new_labels.len() > 1 {
            SelectionResult::Continue(Selector {
                prefix: {
                    let mut new_prefix = String::new();
                    new_prefix.push_str(&self.prefix);
                    new_prefix.push(ch);
                    new_prefix
                },
                labels: new_labels
            })
        } else {
            match new_labels.first() {
                Some(label) => {
                    SelectionResult::NodeId (label.node_id)
                }
                None => {
                    SelectionResult::Canceled
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn initializes_labels_correctly() {
        let di = DisplayInput {
            shapes: vec![],
            selectors: (0..20).map(|i| (Point2::new(0., 0.), i)).collect()
        };
        let selector = Selector::from_display_input(&di);
        //println!("{:?}", selector.labels.iter().map(|&Label { point: _, node_id: _, text: ref s }| s.to_string()).collect::<Vec<_>>());
        //panic!();
        assert_eq!(
            selector.labels.iter()
            .map(|&Label { point: _, node_id: _, text: ref s }| s)
            .collect::<Vec<_>>(),
            vec!["aa", "ad", "af", "ag", "ah", "aj", "ak", "al", "as", "d", "f", "g", "h", "j", "k", "la", "ll", "s"]);
    }
}
