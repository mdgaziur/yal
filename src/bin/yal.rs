use yal::interp::Interpreter;
use yal::session::Session;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let file_name = &args[1];
    let file_content = std::fs::read_to_string(file_name).unwrap();
    let mut session = Session::from_file(
        file_name.clone(),
        file_content,
        args.contains(&String::from("--debug")),
    );
    let mut interpreter = Interpreter::new(&mut session);
    if interpreter.init() {
        interpreter.interpret();
    }
}
