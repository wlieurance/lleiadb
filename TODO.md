# To Do

1. Remove roadblocks (if possible) for parallel execution of indicators in
lpi\_calc.R.  Parallel code execution is already an option, but seems to be
hampered by having to transfer the raw data to each thread. This may not be
fixable but would be nice if it worked.
2. Increase calculation speed of final plot averaging in lpi\_calc.R using
(perhaps?) multidplyr or something similar.
3. Decrease number of errors in import.R by pre-filtering data that does not
match a foreign key BEFORE insertion.
4. Pre-filter data that violates a primary key BEFORE insertion in order to
reduce load times for databases that contain data already in the database as
well as new data.
