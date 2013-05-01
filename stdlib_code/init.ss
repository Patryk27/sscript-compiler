function<void> prepare_console() naked
{
 :CODE
 {
  push(300)
  push(90)
  push(30)
  push(90)
  icall("output.set_size")

  push(false)
  icall("output.set_buffered")
 }
}

public function<void> init() [label="__init"]
{
 prepare_console();
}