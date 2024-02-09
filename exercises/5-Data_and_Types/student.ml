type student = {first_name : string; last_name : string; gpa : float}

let some_stu = {
  first_name= "Alice";
  last_name = "Wihte";
  gpa = 4.
}

let name s = (s.first_name , s.last_name)

let make_student fl ll gpa = {
  first_name = fl;
  last_name = ll;
  gpa = gpa
}