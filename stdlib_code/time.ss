/*
 SScript Standard Library
 time.ss
*/

@visibility("public")

namespace std
{

type<int> Time;

function<int> get_milliseconds() naked
{
 :CODE
 {
  icall("time.get_milliseconds")
  pop(ei1)
 }
}

function<int> get_millis() naked
{
 :CODE
 {
  icall("time.get_milliseconds")
  pop(ei1)
 }
}

function<Time> get_time() naked
{
 :CODE
 {
  icall("time.get_time")
  pop(ef1)
 }
}

function<int> get_seconds(Time time)
{
 // @TODO
}

function<void> sleep(int milis) naked
{
/*
 finish_time = get_millis()+milis;

 while (get_millis() < finish_time); // (do nothing)
*/

 :CODE
 {
  sub(stp, 1)

  pop(ei1) // ei1 = milis

  icall("time.get_milliseconds")
  pop(ei2) // ei2 = current time

  add(ei2, ei1) // ei2 = finish time

 &loop:
  icall("time.get_milliseconds")
  pop(ei1) // ei1 = current time

  if_l(ei1, ei2) // if (ei1 < ei2)
  tjmp(:&loop)   //  goto loop;

  add(stp, 2)
 }
}

}
