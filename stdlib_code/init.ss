function<void> print(any text) naked
{
 :CODE
 {
  sub(stp, 1)
  icall("output.print")
  add(stp, 2)
 }
}

function<void> println(any text) naked
{
 :CODE
 {
  sub(stp, 1)

  icall("output.print")
  push(#10)
  icall("output.print")
  push(#13)
  icall("output.print")

  add(stp, 2)
 }
}

function<void> on_exception()
{
 var<string> msg;
 // var<int[]> callstack_addr;
 // var<int[]> callstack_lines;
 // var<string[]> callstack_files;

 :CODE
 {
  icall("vm.get_last_exception")
  pop(%msg)

  /*
   val = stack_pop();

   if (val is callstack_ref)
   {
    
   }
  */
 }

 println("Exception raised:");
 println(msg);

 :CODE stop()
}

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

function<void> set_default_handler()
{
 var<function> func = on_exception;
 :CODE
 {
  push(%func)
  icall("vm.set_exception_handler")
 }
}

public function<void> init() [label="__init"]
{
 prepare_console();
 set_default_handler();
}