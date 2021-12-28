data blob;
set sashelp.birthwgt(firstobs=1 obs=3);
run;

proc print data=blob;
run;

proc print data=sashelp.birthwgt(firstobs=1 obs=4);
run;
