General Commands:
----
Record
<Code>
{Generate}
Play

Comments:
----
Mute("Comments")


Var:
----
Sound('<var>') as (<expr>)
Sound('<var>') as <value>
Sound('<var>') as '<var>


Conditionals:
----
If()
	<Code>
Else
	<Code>
Close


Loops:
----
Loop
	<Code>
	If()
		Stop
	Else
	Close
EndLoop


I/O:
----
Display(<value>, ...)
Ask('<var>)



Note:
---
Note(<pitch>,<start>,<duration>,<volume>)

Rhythm: (optional)
----
ChangeTempo(<Int>)



