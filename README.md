# Final Project of COBOL
  This is a COBOL program which accepts three unsorted input files which contain the inventory records of a company which manufactures medicines.
  It then outputs a detailed summary report of this company's inventories. It was the final project of my CS-370 (COBOL) class. This program
  processes input files as outlined in program3-description. Firstly, the input files are sorted and then merged. Then the resultant merged
  file is used as an input file to output the summary report. This program makes use of a table to expand a vendor id (The vendor id is 
  stored as one alphabet in the input file), an array, multi-level control breaks (major break on Warehouse id, intermediate break on vendor id,
  and minor break on product id), and input validation. It then outputs the summary report as outlined in program4-printer-spacing-chart.
  If the warehouse id is invaild (i.e. neither NV10 nor CA20 and WA30), the entire record is sent to the error file.
  

