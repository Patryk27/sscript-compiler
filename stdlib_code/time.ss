/*
 SScript Standard Library
 time.ss
*/

public;

namespace std
{
 type<int> Time;

 function<int> get_milliseconds() naked
 {
  :CODE
  {
   icall("time.get_milliseconds")
   pop(ei0)
  }
 }

 function<int> get_millis() naked
 {
  :CODE
  {
   icall("time.get_milliseconds")
   pop(ei0)
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
  return -1;
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

   pop(ei0) // ei0 = milis

   icall("time.get_milliseconds")
   pop(ei1) // ei1 = current time

   add(ei1, ei0) // ei1 = finish time

  &loop:
   icall("time.get_milliseconds")
   pop(ei0) // ei0 = current time

   if_l(ei0, ei1) // if (ei0 < ei1)
   tjmp(:&loop)   //  goto loop;

   add(stp, 2)
  }
 }
}