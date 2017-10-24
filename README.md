== Shift Scheduler ==
Authors: Corey Jack Wilson, Lyndon Won
== What is the problem? ==
For a large majority of workplaces, scheduling shifts is handled using charts or spreadsheets. This form of scheduling can become quite complex, especially when dealing with multiple departments and  large teams. Moreover, current scheduling solutions require explicit knowledge in order manipulate data which makes adjusting them difficult and time-consuming. We plan to build a scheduling system that allows users to ask questions about the schedule. The program will take a list of shifts with information on time, department, role and employee, which the program can interact with to perform different natural language queries and adjustments.

== What are the necessary user stories we have addressed? ==
* As an employee:
** I can ask questions about my current shifts on the schedule
** I can ask questions about who the manager is during my shift
* As a supervisor:
** I can ask questions about which employees are working during a current time on the schedule.
** I can ask questions about which employees are working in a department during a current time on the schedule.
** I can find out which employees are not working during a certain time so I can find someone to cover time off.

== What is the something extra? ==
We will attempt to address several other use cases:
* As an employee, I can schedule myself to work during a time that is not currently on the schedule
* As a manager:
** I can change the shift and day of an employee (as an extension, you could swap the shifts of two or more employees)
** I can promote an employee to a manager
** I can change an employees department

== Known Issues/Bugs ==
1. The 'who is' questions can only take 1 relation, otherwise it returns false.
* Seems to be caused by the lack of a subject (employee, manager) so the second relations uses the noun from the first relation as its subject.
* Example: <code>ask([who,is,working,on,monday,working,in,the,morning],A).</code> will try to prove <code>works_in(monday,morning).</code> instead of <code>works_in(_some_employee,morning).</code>
2. Cannot have more than 2 relations for any type of question.
* Happens when relation types are different and when they are all the same.
* Example: <code>ask([what,manager,works,on,monday,works,on,tuesday,works,on,friday],E).</code>

== Change Notes ==
1. We moved the following user stories to 'Something Extra' because we felt it went further beyond the what was learned in class and wanted to do it the right way by actually persisting the data (instead of merely mutating the program).
* As an employee, I can schedule myself to work during a time that is not currently on the schedule
* As two employees, we can trade shifts
2. We dropped the following 'Something Extra's' because we were blocked and couldn't figure out restrictions within the natural language interface and wanted to work on persisting the data properly, which we felt was more important to an actual scheduling system then having restrictions.
* As an employee, I can have custom requirements for when I switch shifts (i.e. that i never go below 40 hours/week, that I never schedule myself for a time when I am busy on an external calendar)
* As a supervisor, I can have custom requirements for when employees switch shifts (i.e. that two employees must be in the same department, or have the same training)
3. We changed the following user story to more accurately describe a possible user requirement
* I can have employees suggested to me for when another employee takes time off -> I can find out which employees are not working during a certain time so I can find someone to cover time off.
* As two employees, we can trade shifts -> As a manager, I can swap the shifts of two employees

== What did we learn from doing this? ==
'''Does the code work?'''
As far as code quality, we wrote a test suite for several queries in the basic user stories and for the advanced user stories. The coverage shows that the functionality works, although there are two known bugs for which they are documented and their tests have been commented out. Please make sure you run any manual tests against a default database.

'''Is the extra part interesting? Does it give evidence for the usefulness of what you have done?'''
We found the extra part to be interesting because it shows how succinctly Prolog can handle the description of a highly generalized schedule while at the same time preserving readability for the end user. We chose the extra part because people typically think of functional/logic languages as being constrained to an immutable environment of rules. While elegant in its own right, by showing that Prolog has the additional ability to mutate a simple db, its functionality is greatly expanded beyond domain conceptualization and knowledge graphs. From the extra feature, one could envision a natural language parser that translates directly to SQL (or any other kind of NoSql API for that matter). Then, when combined with additional libraries for audio interpretation, one could realize a fairly simple speech-to-SQL system that would be lightweight, relatively maintainable, easily extensible and highly usable.

'''Is the code readable and well documented? Does it give the intended interpretation for all symbols? Would someone else be able to take your code and build on it?'''
The code is separated into coherent sections. One for the logic for actions, questions, general rules of the KB, and the testing suite.
The interpretations of each of the symbols is obvious because we chose clean coder naming as a convention.
The code is extensible insofar as if one wanted to use this in production, they could fairly obviously add additional departments, roles, shifts, actions, and hires just by following the patterns.
Further extension of this scheduler should consider:
* the ability to add in 'one-off' shifts that need not follow any kind of rule (i.e. Working on Saturday, October 30th, 12-4, one time) and the ability to aggregate total hours based on this.
* the ability to add in constraints (i.e. you cannot schedule someone on X-mas, you cannot schedule someone into Grocery that hasn't been trained on products, etc).
* the ability to visualize coverage in some graphical way.

'''Is the conclusion on the feasibility of your approach justiﬁed by the evidence?'''
As far as feasibility of using Prolog for such a task, we conclude that it is sufficient as a language for scheduling, natural language parsing, and even to some degree, simple database management. The main concern lies in how integrated Prolog could be with a modern RDBMS in order to take advantage of all their advanced querying features. Since most business data is in some for of relational database already, it might not make sense to translate this data to Prolog facts for the mere sake of utilizing Prolog as a NL interface. That said, as mentioned earlier, if one could utilize the NL parser to create SQL statements, this might be a more suitable task for Prolog rather then recreating the DB. Our main pain points were in coding in relations and constraints for the data and how coupled they are to the natural language logic. While having constraints deeply embedded in the NL logic makes sense, engraining -database- constraints into the NL logic seems to defy good design principles (i.e. having to modify a noun fact whenever a new noun is added). Thus, we summarize that while Prolog is suitable to the task of NLP and querying, it might be too far to extend this to a full RDBMS (which most production schedules and data sets are already contained in).

== Source Code ==
[https://github.com/coreyjackwilson/natural-language-scheduler Github Repository]
