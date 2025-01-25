use lox::interpret::{run, Environment};

#[test]
pub fn test() {
	let input = r#"
var a = "global";
{
  fun showA() {
    print a;
  }

  showA();
  var a = "block";
  showA();
}"#;

	let mut environment = Environment::default();

	if let Err(error) = run(input, &mut environment, true) {
		println!("{error:#}");
	}
}
