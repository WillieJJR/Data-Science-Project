/****** Script for SelectTopNRows command from SSMS  ******/

--Query below shows the total number of unique products sold in 2015 (219)
/*SELECT TOP (1000) 
      COUNT(DISTINCT([Stock Item Key]))
  FROM [WideWorldImportersDW].[Fact].[Sale]
  WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015' */

  --Query below shows average sales volume of all products
/*SELECT AVG(b.sum_of_quantity) AS 'Average Quantity purchased'
FROM (
	SELECT [Stock Item Key], SUM(Quantity) AS 'sum_of_quantity'
	  FROM [WideWorldImportersDW].[Fact].[Sale]
	  WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015' 
	  Group By [Stock Item Key]
	  ) AS b --Average Quantity Purchased for all stock items in 2015 is 12,512. Using this to determine what is "weak" sales volume 
	  */
--Consider removing any products that didn't sell at least 50% of the Average sales volume 

--Query below shows the aggregated average unit profit between all products Average unit profit
/*SELECT AVG(b.[AVG Unit Profit]) AS 'Average All Unit Profits'
FROM (
	SELECT [Stock Item Key], AVG(Profit/Quantity) AS 'AVG Unit Profit'
	  FROM [WideWorldImportersDW].[Fact].[Sale]
	  WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015' 
	  Group By [Stock Item Key]
	  ) AS b --Average Unit Profit for all stock items in 2015 is $22.30. Using this to determine what is "weak" profit margins
	  */
--Consider removing any products that didn't have margins less than 50% of the Average profit margins

SELECT c.*,
		CASE
			WHEN  Volume_Sold_At_Risk = 'At-Risk' AND Low_Profit_At_Risk = 'At-Risk'
			THEN 'Eliminate Product'
			ELSE 'OK'
		END AS Item_At_Risk
FROM (
	SELECT  b.*,
			CASE 
				WHEN (b.[SUM of Quantity] < 6256) 
				THEN 'At-Risk'
				ELSE 'OK'
			END AS Volume_Sold_At_Risk,
			CASE
				WHEN (b.[AVG Unit Profit] < 11.15)
				THEN 'At-Risk'
				ELSE 'OK'
			END AS Low_Profit_At_Risk
	FROM (
		SELECT [Stock Item Key], 
			  SUM(Quantity) AS 'SUM of Quantity'
			  , AVG(Profit/Quantity) AS 'AVG Unit Profit'
		  FROM [WideWorldImportersDW].[Fact].[Sale]
		  WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015'
		  GROUP BY [Stock Item Key]
		  --The last customer purchase date where a product was recieved was 05/31/2016, so we will use the last full year prior, 2015
		  ) AS b
	) AS c -- 219 unique total products sold in 2015
