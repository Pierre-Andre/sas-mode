data blob;
set sashelp.birthwgt(firstobs=1 obs=3);
run;

proc print data=blob;
run;
