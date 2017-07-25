SynergyFinder technical documentation. 

Application notes:

	Input data:
	
	Two possible input file formats (Table and Matrix) are allowed in SynergyFinder.


		The input table must comprise following columns:
				1. PairIndex - index of drug-pair to be analyzed
				4. Response - %inhibition or %viability values
				5. Drug1 - first drug name
				6. Drug2 - second drug name
				7. Conc1 - concentration of Row drug 
				8. Conc2 - concentration of Column drug
				9. ConcUnit - unit of concentrarion		
		The expected annotation file format is either *.xlsx (preferably) or *.csv.


			

	1.	Visualization of dose-response data 

			Visualization of %inhibition dose-response matrix and dose-response curves for both drugs ((i.e. the first column and first row in the dose-response matrix), fitted by four-parameter logistic curves.). 				
					

	2.	Visualization of synergy scores
			
			input parameters:
					
					method - a reference model of non-interaction by means of which the expected effect (response) will be calculated.  The deviation between expected effect and observed will result in synergy scores and interaction maps.
					correction - a baseline correction, trying to fix negative or irregular responses.
			
			output data:
			
					2D and 3D interaction maps reflecting synergistic and antagonistic dose regions.

					
	3.  Report generation:

			input parameters:
					
					user must specify which types of plots and which drug combinations should be included in the report (by default, all plots are included).
					
			output:
			
					The static/dynamic pdf report. Dynamic pdf report allows rotation of 3D interaction maps, in order to find the best side for printing.
													* in case of problems with dynamic report, use Adobe Reader.


Example Data:

	1. Annotation file - exampleTable.xlsx/exampleTable.csv/exampleTable.txt is the example file for input Annotation file in "Table" format.   
		     exampleMatrix.xlsx/exampleMatrix.csv/exampleMatrix.txt is the example file for input Annotation file in "Matrix" format. 
	2. Readout for example data: Inhibition													


												
Author: 
	Ianevski Aleksandr <aleksandr.ianevski@helsinki.fi>
		
Acknowledgements: 
	Liye He, Tero Aittokallio, and Jing Tang							
								
 

