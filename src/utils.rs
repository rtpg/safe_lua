#[macro_export]
macro_rules! file_contents {
    ($filename:expr, $into_var:ident) => {
        let mut $into_var = String::new();
        {
            let mut file = File::open($filename).unwrap();
            file.read_to_string(&mut $into_var).unwrap();
        }
    };
}
