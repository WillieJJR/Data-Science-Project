/****** Script for SelectTopNRows command from SSMS  ******/

/*SELECT  b.Customer,
			SUM(a.[Quantity]) AS 'SUM of Quantity Purchased',
			SUM(a.Profit) AS 'Total Profit'
	FROM [WideWorldImportersDW].[Fact].[Sale] a
	inner join [WideWorldImportersDW].[Dimension].[Customer] b
	ON a.[Customer Key] = b.[Customer Key]
	WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015'AND b.Customer NOT IN ('Unknown')
	GROUP BY a.[Customer Key], b.Customer
--402 Unique customers purchased products in 2015
--Removing the Product purchases of Unknown Customers ~1.05M purchases and $10.4M of purchases
*/

/*SELECT AVG(ab.[SUM of Quantity Purchased]) AS 'AVG Quantity Purchased'
FROM (
	SELECT  b.Customer,
			SUM(a.[Quantity]) AS 'SUM of Quantity Purchased',
			SUM(a.Profit) AS 'Total Profit'
	FROM [WideWorldImportersDW].[Fact].[Sale] a
	inner join [WideWorldImportersDW].[Dimension].[Customer] b
	ON a.[Customer Key] = b.[Customer Key]
	WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015'AND b.Customer NOT IN ('Unknown')
	GROUP BY a.[Customer Key], b.Customer
	) ab
--The Average amount of products purchased in 2015 was 4210
--Potnetially eliminate any company that has purchased less than 50% of the average quantity purchased
*/

/*SELECT AVG(ab.[Total Profit]) AS 'AVG Profit'
FROM (
	SELECT  b.Customer,
			SUM(a.[Quantity]) AS 'SUM of Quantity Purchased',
			SUM(a.Profit) AS 'Total Profit'
	FROM [WideWorldImportersDW].[Fact].[Sale] a
	inner join [WideWorldImportersDW].[Dimension].[Customer] b
	ON a.[Customer Key] = b.[Customer Key]
	WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015'AND b.Customer NOT IN ('Unknown')
	GROUP BY a.[Customer Key], b.Customer
	) ab
--The Average amount of profit is $41,165
--Potnetially eliminate any company that has less than 50% of the average profit
*/


SELECT e.*
FROM (
	SELECT d.*,
			CASE
				WHEN Volume_Purchased_At_Risk = 'At-Risk' AND Customer_Profit_At_Risk = 'At-Risk'
				THEN 'Eliminate Customer'
				ELSE 'OK'
			END AS Customer_At_Risk
	FROM (	
		SELECT c.*,
				CASE 
					WHEN (c.[SUM of Quantity Purchased] < (4210/2))
					THEN 'At-Risk'
					ELSE 'OK'
				END AS Volume_Purchased_At_Risk,
				CASE 
					WHEN (c.[Total Profit] < (41165/2))
					THEN 'At-Risk'
					ELSE 'OK'
				END AS Customer_Profit_At_Risk
		FROM (	
			SELECT  b.Customer,
					SUM(a.[Quantity]) AS 'SUM of Quantity Purchased',
					SUM(a.Profit) AS 'Total Profit'
			FROM [WideWorldImportersDW].[Fact].[Sale] a
			inner join [WideWorldImportersDW].[Dimension].[Customer] b
			ON a.[Customer Key] = b.[Customer Key]
			WHERE DATEPART(YEAR, [Invoice Date Key]) = '2015' AND b.Customer NOT IN ('Unknown')
			GROUP BY b.Customer
			) c
		) d
	) e
WHERE e.Customer_At_Risk = 'Eliminate Customer'
--5 Customers to be recommended to be removed (Tailspin Toys (Howells, NE), Wingtip Toys (Bethel Acres, OK), Wingtip Toys (Miesville, MN), Wingtip Toys (Munich, ND), Wingtip Toys (Portales, NM))