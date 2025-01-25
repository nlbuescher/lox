use lox::interpret::{run, Environment};

#[test]
pub fn test() {
	let input = r#"
fun test(value) {
	print value;
	return;
	for (var i = 0; i < 10; i = i + 1) {
		print i;
	}
}
print test("test");

fun makeCounter() {
	var i = 0;
	fun count() {
		i = i + 1;
		print i;
	}

	return count;
}
var counter = makeCounter();
counter();
counter();
counter();"#;

	let mut environment = Environment::default();

	if let Err(error) = run(input, &mut environment, true) {
		println!("{error:#}");
	}
}
