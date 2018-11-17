#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "rwmake.ch" 
#INCLUDE "TOTVS.CH"   

#DEFINE  CLRF  CHR ( 13 ) + CHR ( 10 ) 

/*
+-----------+------------+----------------+-------------------+-------+---------------+
| Programa  | FSCOMR01   | Desenvolvedor  | Catharina Oliveira| Data  | 19.07.2016    |       
+-----------+------------+----------------+-------------------+-------+---------------+
| Descricao | Mapa de Cotacoes                                                        |
+-----------+-------------------------------------------------------------------------+
| Modulos   | SIGACOM                                                                 |
+-----------+-------------------------------------------------------------------------+
|           ALTERACOES FEITAS DESDE A CRIACAO                            		      |
+----------+-----------+--------------------------------------------------------------+
|Autor     | Data      | Descricao                                       			  |
+----------+-----------+--------------------------------------------------------------+  
|          |           |                                                     	   	  |
+----------+--------------------------------------------------------------------------+
*/

User Function FSCOMR01()
***************************************************************************************
* 
*
*****             

	Local lValida	:= .T.
	
	Private xLinha
	Private cPerg	:= "FSCOMR01"	
	Private aDados1 := {}
	Private aDados2 := {}    
	Private aDados3 := {}
	
	Private cFilDe  := SC8->C8_FILIAL 	//Space(02)
	Private cCotaDe := SC8->C8_NUM 		//Space(06)        
       
	// Busca Informações e Alimenta Vetores de Dados 
	Processa( {|| lValida := BuscaInfo() },"Aguarde...","Buscando informações...")	                           	
	
	// Efetua a impressão em Tela. 
	If lValida
		ImpMapa()
	Else
		MsgBox("Limite de 3 Fornecedores por cotação ultrapassado. Revise sua análise e tente novamente.","Operação Inválida!","INFO")	
	EndIf
	         

Return 


Static Function BuscaInfo()
***************************************************************************************
* 
*
*****
    
	Local nX		:= 1  
	Local cDtSolic	:= ""
	Local cQuery 	:= ""  
	Local cSCItem	:= ""	// Chave 
	Local cExisForn	:= ""
	Local cItem		:= ""	
	Local cCCusto	:= ""
	Local cUM		:= ""	
	Local nQtde		:= ""
	Local cDescPr	:= ""		
	Local cObsrPr	:= ""	
	Local nPosY	 	:= 0  
	Local nFornece	:= 3   
	Local nColF		:= 3 
	Local nFrete	:= 0			
	Local nPreco1	:= 0
	Local nPreco2	:= 0
	Local nPreco3	:= 0
	Local nTotal1	:= 0	
	Local nTotal2	:= 0	
	Local nTotal3	:= 0 
	Local nPrcFor	:= 0
	Local nTotFor	:= 0		
	
	ProcRegua(8)  
 	IncProc() 
 	IncProc()		
			
	aDados1 :=	{	{""		   					,""			,""				,""				,""				},;    
					{""		   					,""			,"FORNECEDOR A"	,"FORNECEDOR B"	,"FORNECEDOR C"	},;  
					{"CENTRO DE CUSTO:"			,""			,""				,""				,""				},;
					{"DATA SOLICITAÇÃO:"   		,""			,"Telefone: "	,"Telefone: " 	,"Telefone: "	},; 
					{"PEDIDO POR:"		    	,""			,"Contato: "    ,"Contato: "    ,"Contato: "	}}                                        

	aDados3 :=	{	{"ORÇADO POR:"				,""			,"SUBTOTAL"		,0 ,"SUBTOTAL"	 ,0 ,"SUBTOTAL"	  ,0 ,"SUBTOTAL" ,0 },;
					{"DATA DO ORÇAMENTO:"		,""			,"COND.PAGT"	,'',"COND.PAGT"  ,'',"COND.PAGT"  ,'',""		 ,""},;
					{"APROVADO - NOME E DATA:"	,""	   		,"PRAZ.ENT "	,0 ,"PRAZ.ENT "  ,0 ,"PRAZ.ENT "  ,0 ,""	 	 ,""},;
					{"FORNECEDOR A"				,"ITENS "	,"DESCONTO"		,0 ,"DESCONTO"	 ,0 ,"DESCONTO"	  ,0 ,""		 ,""},;
					{"FORNECEDOR B"				,"ITENS "	,"TOTAL"		,0 ,"TOTAL"	     ,0 ,"TOTAL"	  ,0 ,"TOTAL"	 ,0 },;   
					{"FORNECEDOR C"				,"ITENS "	,"FRETE"		,0 ,"FRETE"		 ,0 ,"FRETE"	  ,0 ,""		 ,0 }}
              																		
	cQuery := " SELECT C8_NUM,C8_NUMSC NUMSC,C8_ITEM ITEM,C8_UM,C8_QUANT,C8_PRODUTO,B1_DESC, 		 	"+ CRLF
	cQuery += " 		C8_FORNECE+C8_LOJA FORNECEDOR, A2_NOME, A2_DDD+' '+A2_TEL TEL, C8_CONTATO,		"+ CRLF
	cQuery += "			C8_PRECO,C8_TOTAL,C8_TPFRETE,C8_TOTFRE,C8_AVISTA,								"+ CRLF   
	cQuery += "			cast(cast(C8_OBS as varbinary(8000)) as varchar(8000)) C8_OBS,					"+ CRLF  	
	cQuery += "			(C8_PRODUTO+cast(cast(C8_OBS as varbinary(8000)) as varchar(8000))+C8_UM+C8_ITEM)CHAVE,		"+ CRLF  
	cQuery += "			ISNULL(E4_DESCRI,'') E4_DESCRI,C8_PRAZO AS PRAZO_DIAS, C8_VLDESC DESCONTO_VLR,  "+ CRLF	
	cQuery += "			C8_DESC DESCONTO_PERC, C8_DESC1, C8_DESC2, C8_DESC3, C1_EMISSAO, C1_CC			"+ CRLF
	cQuery += "	FROM "+RetSqlName("SC8")+" SC8 															"+ CRLF
	cQuery += "		JOIN "+RetSqlName("SA2")+" SA2 ON A2_COD=C8_FORNECE AND A2_LOJA=C8_LOJA AND SA2.D_E_L_E_T_=''	"+ CRLF
	cQuery += "		JOIN "+RetSqlName("SB1")+" SB1 ON C8_PRODUTO = B1_COD AND SB1.D_E_L_E_T_=''			"+ CRLF    
	cQuery += "		JOIN "+RetSqlName("SC1")+" SC1 ON C8_NUMSC=C1_NUM AND C1_FILIAL=C8_FILIAL 			"+ CRLF
	cQuery += "			AND C1_PRODUTO=C8_PRODUTO AND C1_ITEM=C8_ITEMSC	AND SC1.D_E_L_E_T_=''  			"+ CRLF	
	cQuery += "		LEFT JOIN "+RetSqlName("SE4")+" SE4 ON C8_COND = E4_CODIGO AND SE4.D_E_L_E_T_=''	"+ CRLF
	cQuery += "	WHERE SC8.D_E_L_E_T_=''		   				 	 		 	"+ CRLF
	cQuery += "		AND C8_FILIAL = '"+cFilDe+"' 							"+ CRLF 
	cQuery += "		AND C8_NUM = '"+cCotaDe+"' 								"+ CRLF              
	cQuery += "	ORDER BY C8_PRODUTO+cast(cast(C8_OBS as varbinary(8000)) as varchar(8000))+C8_UM+C8_ITEM,C8_FORNECE+C8_LOJA			"+ CRLF	
	
	TCQuery cQuery New Alias "TMP01"

 	IncProc() 
 	IncProc()
	
	dbSelectArea("TMP01")
	dbGoTop()  

 	cSCItem := AllTrim(TMP01->CHAVE)   	
	nPrcFor	:= TMP01->C8_PRECO
	nTotFor	:= TMP01->C8_TOTAL   
	nFrete	:= TMP01->C8_TOTFRE
	cDtSolic:= TMP01->C1_EMISSAO
	cCCusto := AllTrim(TMP01->C1_CC)

	While !Eof()       
		
		//------------------------ Alimenta Parte I - Fornecedores ------------------------ 
		If !(TMP01->FORNECEDOR $ cExisForn)  

	   		// Limite de 3 Fornecedores por Cotação, ultrapassado => Inconsistência.
	   		If nFornece > 5
		   		TMP01->(dbCloseArea())
		   		Return(.F.)                                  
			EndIf
		
			aDados1[01,nFornece] := AllTrim(TMP01->FORNECEDOR)
			aDados1[03,nFornece] += LEFT(AllTrim(TMP01->A2_NOME),24)			
			aDados1[04,nFornece] += AllTrim(TMP01->TEL)			
			aDados1[05,nFornece] += AllTrim(TMP01->C8_CONTATO)									
			                                                
			cExisForn += "_"+AllTrim(TMP01->FORNECEDOR)    
	   		++nFornece	 
	   		    		
	   	EndIf 	    
		
		If TMP01->C1_EMISSAO < cDtSolic
			cDtSolic:= TMP01->C1_EMISSAO	
		EndIf
		                                          
        //-------------------- Alimenta Parte II - Produtos e Orçamentos------------------- 
        If cSCItem <> AllTrim(TMP01->CHAVE)
        
		    aAdd(aDados2,{cItem				,;
						  cUM  				,;
						  Alltrim(Transform(nQtde,"@E 99,999.99")),;
						  Capital(cDescPr)	,;
						  Capital(cObsrPr)	,;
						  Alltrim(Transform(nPreco1,"@E 99,999.99")),;
						  Alltrim(Transform(nTotal1,"@E 99,999.99")),;
						  Alltrim(Transform(nPreco2,"@E 99,999.99")),;
						  Alltrim(Transform(nTotal2,"@E 99,999.99")),;
						  Alltrim(Transform(nPreco3,"@E 99,999.99")),;
						  Alltrim(Transform(nTotal3,"@E 99,999.99")),;
						  Alltrim(Transform(nPrcFor,"@E 99,999.99")),;
						  Alltrim(Transform(nTotFor,"@E 99,999.99"))}) 
						    
			nPreco1 := 0						  
       		nTotal1 := 0						  
			nPreco2 := 0						  
       		nTotal2 := 0
			nPreco3 := 0					  
       		nTotal3 := 0
       		nPrcFor := TMP01->C8_PRECO
       		nTotFor := TMP01->C8_TOTAL 
       		
   			cSCItem := AllTrim(TMP01->CHAVE)    		       		
		
		EndIf
		
		cItem	:= TMP01->ITEM
		cUM		:= TMP01->C8_UM
		nQtde	:= TMP01->C8_QUANT
		cDescPr	:= LEFT(AllTrim(TMP01->B1_DESC),41)	
		cObsrPr	:= LEFT(AllTrim(TMP01->C8_OBS),44)
		
     	If aDados1[01,03] == AllTrim(TMP01->FORNECEDOR)    
        	nPreco1 := TMP01->C8_PRECO
       		nTotal1 := TMP01->C8_TOTAL       		
       		nColF	:= 04 						   	       		
		ElseIf aDados1[01,04] == AllTrim(TMP01->FORNECEDOR)  
        	nPreco2 := TMP01->C8_PRECO
       		nTotal2 := TMP01->C8_TOTAL  
       		nColF	:= 06       		        			       		
  		Else
        	nPreco3 := TMP01->C8_PRECO
       		nTotal3 := TMP01->C8_TOTAL                    
       		nColF	:= 08        		       		       		
	    EndIf 			
        
        If (TMP01->C8_PRECO > 0 .And. (TMP01->C8_PRECO <  nPrcFor .Or. nPrcFor == 0))
			nPrcFor	:= TMP01->C8_PRECO// Min(TMP01->C8_PRECO,nPrcFor)  
		EndIf  
        If (TMP01->C8_TOTAL > 0 .And. (TMP01->C8_TOTAL <  nTotFor .Or. nTotFor == 0))
			nTotFor	:= TMP01->C8_TOTAL// Min(TMP01->C8_TOTAL,nTotFor)
		EndIf  		
	 	
		//-------------------- Alimenta Parte III - Subtotais e Negociação----------------- 
		aDados3[01,nColF] += TMP01->C8_TOTAL
       	aDados3[02,nColF] := TMP01->E4_DESCRI 
       	aDados3[03,nColF] := Max(TMP01->PRAZO_DIAS,aDados3[03,nColF])    
		aDados3[04,nColF] += TMP01->DESCONTO_VLR   
		nFrete := Min(TMP01->C8_TOTFRE,nFrete)  
		aDados3[06,nColF]  := TMP01->C8_TOTFRE   
		aDados3[05,nColF]  := aDados3[01,nColF] - aDados3[04,nColF]  	        
 		    
		dbSelectArea("TMP01")
		dbSkip()
	EndDo		
	
	TMP01->(dbCloseArea()) 
   	IncProc() 
 	IncProc()
 	
	aDados1[03,02] := cCCusto+' - '+AllTrim(Posicione("CTT",1,xFilial("CTT")+cCCusto,"CTT_DESC01")) 	
	aDados1[04,02] := DtoC(StoD(cDtSolic))
			
    aAdd(aDados2,{cItem				,;                               // 01
				  cUM  				,;                               // 02
				  Alltrim(Transform(nQtde,"@E 99,999.00")),;         // 03
				  Capital(cDescPr)	,;                               // 04
				  Capital(cObsrPr)	,;                               // 05
				  Alltrim(Transform(nPreco1,"@E 99,999.99")),;       // 06
				  Alltrim(Transform(nTotal1,"@E 99,999.99")),;       // 07
				  Alltrim(Transform(nPreco2,"@E 99,999.99")),;       // 08
				  Alltrim(Transform(nTotal2,"@E 99,999.99")),;       // 09
				  Alltrim(Transform(nPreco3,"@E 99,999.99")),;       // 10
				  Alltrim(Transform(nTotal3,"@E 99,999.99")),;       // 11
				  Alltrim(Transform(nPrcFor,"@E 99,999.99")),;       // 12
				  Alltrim(Transform(nTotFor,"@E 99,999.99"))})       // 13 	

	// Subtotal e Total Geral
	For nX := 1 To Len(aDados2)
		aDados3[01,10] += Val( Replace(Replace(aDados2[nX,13],".",""),",","."))		
	Next nX

	aDados3[06,10] :=  nFrete

 	IncProc() 
 	IncProc()
										
Return(.T.)

            

Static Function ImpMapa()
***************************************************************************************
* 
*
*****

	Local nX		:= 1  
	Local nTamVet	:= 28
	Local cEmpresa	:= Capital(Posicione("SM0",1,cEmpAnt+"01","M0_NOMECOM") )
	Local cCnpj 	:= Transform(AllTrim(SM0->M0_CGC),"@R 99.999.999/9999-99") 	
	Local lProxPag	:= .F.
	  
	Private oFntNegrito:= TFont():New("MS Mincho",,-10,,.T.,,,,,.F.,.F. )      
	Private oFont1Sub  := TFont():New("MS Mincho",,-10,,.F.,,,,,.T.,.F. ) // Sublinhado
	
	Private oPrint  
	Private nPag	:= 1
	Private oFont0 	:= TFont():New("MS Mincho",,11,,.F.,,,,,.F.)
	Private oFont1 	:= TFont():New("MS Mincho",,09,,.F.,,,,,.F.)
	Private oFont2 	:= TFont():New("MS Mincho",,09,,.T.,,,,,.F.)
		
	oPrint := TMSPrinter():New(OemToAnsi("MAPA DE COTAÇÃO - COMPRAS"))
	oPrint:Setup( )
	oPrint:SetPaperSize(9)   
	oPrint:SetLandscape()
	oPrint:StartPage() 	
   
 	xLinha := 80       
	oPrint:Box(xLinha,30,xLinha + 70,3400)
	oPrint:Say(xLinha+20,1600,OemToAnsi("MAPA DE COTAÇÃO"),oFont2,130)      
	
	xLinha+=70	
	
	oPrint:Box(xLinha,0030,xLinha + 050,0400)		//	Empresa    
	oPrint:Say(xLinha+11,0040,"EMPRESA:",oFont2,50) 
   	oPrint:Box(xLinha,0400,xLinha + 050,1720)	   	//  Detalhes      	                    	
	oPrint:Say(xLinha+11,0410,OemToAnsi(cEmpresa),oFont1,50) 
		
	oPrint:Box(xLinha,1720,xLinha + 050,3400)     	//  Fornecedores Consultados 
	oPrint:Say(xLinha+10,2300,"FORNECEDORES CONSULTADOS",oFont2,50)  	  	
   	xLinha+=50	                                                   
   	
	oPrint:Box(xLinha,0030,xLinha + 050,0400)		//	CNPJ   	    
	oPrint:Say(xLinha+10,0040,"CNPJ:",oFont2,50)
	oPrint:Box(xLinha,0400,xLinha + 050,1720)	   	//  Detalhes  	   
	oPrint:Say(xLinha+10,0415,OemToAnsi(cCnpj),oFont1,50)	

	oPrint:Box(xLinha,2980,xLinha + 350,3400)
	oPrint:Say(xLinha+95,3050,"FORMADOR DE PREÇO",oFont2,50)

   	For nX := 2 to 4  	
		oPrint:Box(xLinha,0030,xLinha + 50,0400)  
		oPrint:Say(xLinha+10,0040,aDados1[nX,01],oFont2,50)
		oPrint:Box(xLinha,0400,xLinha + 50,1720)	     	  
		oPrint:Say(xLinha+10,0410,aDados1[nX,02],oFont1,50)
		oPrint:Box(xLinha,1720,xLinha + 50,2140)     	      
		oPrint:Say(xLinha+10,1730,Left(aDados1[nX,03],27),oFont1,50)
		oPrint:Box(xLinha,2140,xLinha + 50,2560)		      
		oPrint:Say(xLinha+10,2150,Left(aDados1[nX,04],27),oFont1,50)
        oPrint:Box(xLinha,2560,xLinha + 50,2980)	   	      
		oPrint:Say(xLinha+10,2570,Left(aDados1[nX,05],27),oFont1,50)
		xLinha+=50			   		
  	Next nX    	
  	    	 			
	//---------Cabeçalho dos produtos --------------	
	fCabecalho(@oPrint,.F.)              
	
	//--------- Impressão dos Itens --------------	 
	If Len(aDados2) > nTamVet
		nTamVet := Len(aDados2)
	EndIf	    

	For nX := 1 to nTamVet //36-28                  
	
		If (xLinha > 2200)
			fRodape(@oPrint)
		   	oPrint:EndPage()
			oPrint:StartPage()			
			fCabecalho(@oPrint,.T.)  
		EndIf 	

	    // Desenhando caixas
		oPrint:Box(xLinha,0030,xLinha + 50,0085)    	// Item 
		oPrint:Say(xLinha+10,43,cValtoChar(nX),oFont1,50)			 
	    oPrint:Box(xLinha,0085,xLinha + 50,0140)   		// Unidade
	   	oPrint:Box(xLinha,0140,xLinha + 50,0240)   		// Quantidade    
	   	
	    oPrint:Box(xLinha,0240,xLinha + 50,0960)        // Descrição
	    oPrint:Box(xLinha,0960,xLinha + 50,1720)        // Observação.  
		 
		oPrint:Box(xLinha,1720,xLinha + 50,1900)        // Vlr.Unitário - Fornecedor 1 
		oPrint:Box(xLinha,1900,xLinha + 50,2140)		// Vlr.Total 
			    	
		oPrint:Box(xLinha,2140,xLinha + 50,2320)        // Vlr.Unitário - Fornecedor 2 
		oPrint:Box(xLinha,2320,xLinha + 50,2560)		// Vlr.Total    	 
				
		oPrint:Box(xLinha,2560,xLinha + 50,2740)        // Vlr.Unitário - Fornecedor 3 
		oPrint:Box(xLinha,2740,xLinha + 50,2980)		// Vlr.Total
	   	  
		oPrint:Box(xLinha,2980,xLinha + 50,3160)        // Vlr.Unitário - Formador de Preço
		oPrint:Box(xLinha,3160,xLinha + 50,3400)		// Vlr.Total
	   	
	   	// Imprimindo valores	
		If nX <= Len(aDados2) 
 
		    oPrint:Say(xLinha+10,095,aDados2[nX,02],oFont1,50)        	// UM
			oPrint:Say(xLinha+10,0238,aDados2[nX,03],oFont1,,,,1)  		// Quantidade
			oPrint:Say(xLinha+10,0260,aDados2[nX,04],oFont1,50)  		// Descrição Produto
			oPrint:Say(xLinha+10,0970,aDados2[nX,05],oFont1,50)         // Observação do Produto
  		    oPrint:Say(xLinha+10,1895,aDados2[nX,06],oFont1,,,,1)	    // Unitário 1
			oPrint:Say(xLinha+10,2135,aDados2[nX,07],oFont1,,,,1)		// Total 1
			oPrint:Say(xLinha+10,2315,aDados2[nX,08],oFont1,,,,1)  		// Unitário 2
			oPrint:Say(xLinha+10,2555,aDados2[nX,09],oFont1,,,,1)		// Total 2
		    oPrint:Say(xLinha+10,2735,aDados2[nX,10],oFont1,,,,1)		// Unitário 3
			oPrint:Say(xLinha+10,2975,aDados2[nX,11],oFont1,,,,1)		// Total 3
			oPrint:Say(xLinha+10,3155,aDados2[nX,12],oFont1,,,,1)  		// Unitário Formador Preço
			oPrint:Say(xLinha+10,3395,aDados2[nX,13],oFont1,,,,1) 		// Total Formador Preço

		EndIf  
			   	
		xLinha+=50
		   
	Next nX	
	
	oPrint:Box(xLinha,0030,xLinha + 50,3400)
	xLinha+=50  
	
	If (xLinha > 2255)
		fRodape(@oPrint)
	   	oPrint:EndPage()
		oPrint:StartPage()
		fCabecalho(@oPrint,.T.)	
	EndIf	
   				     
    //------- Forma Pagamento e Negociação--------
	For nX := 1 to 6 
        
        If nX <> 3
			oPrint:Box(xLinha,0030,xLinha + 50,0450) 
			oPrint:Say(xLinha+10,0040,aDados3[nX,01],oFont2,50)
			oPrint:Box(xLinha,0450,xLinha + 50,1720)
			oPrint:Say(xLinha+10,0460,aDados3[nX,02],oFont1,50)		
		Else
			oPrint:Box(xLinha,0030,xLinha + 50,1720) 
			oPrint:Say(xLinha+10,0040,aDados3[nX,01],oFont2,50)	
		EndIf		
			  
		oPrint:Box(xLinha,1720,xLinha + 50,1900) 
		oPrint:Say(xLinha+10,1730,aDados3[nX,03],oFont1,50)
		oPrint:Box(xLinha,1900,xLinha + 50,2140)	
				   	
		oPrint:Box(xLinha,2140,xLinha + 50,2320)    
		oPrint:Say(xLinha+10,2150,aDados3[nX,05],oFont1,50)
	   	oPrint:Box(xLinha,2320,xLinha + 50,2560) 
		
	    oPrint:Box(xLinha,2560,xLinha + 50,2740)           
		oPrint:Say(xLinha+10,2570,aDados3[nX,07],oFont1,50)
		oPrint:Box(xLinha,2740,xLinha + 50,2980)	         
		
		oPrint:Box(xLinha,2980,xLinha + 50,3160)           
		oPrint:Say(xLinha+10,2990,aDados3[nX,09],oFont1,50)		
		oPrint:Box(xLinha,3160,xLinha + 50,3400)  
		
		If nX <> 2 /*.And. nX <> 5*/ .And. nX <> 3   
	
			oPrint:Say(xLinha+10,2135,Alltrim(Transform(aDados3[nX,04],"@E 99,999.99")),oFont1,,,,1)	 //,,,,1): 1=Direita;2=Centralizado;0=Esquerda  		       
			oPrint:Say(xLinha+10,2555,Alltrim(Transform(aDados3[nX,06],"@E 99,999.99")),oFont1,,,,1)
			oPrint:Say(xLinha+10,2975,Alltrim(Transform(aDados3[nX,08],"@E 99,999.99")),oFont1,,,,1) 
			 
		ElseIf nX == 3	
			oPrint:Say(xLinha+10,1910,cValToChar(aDados3[nX,04])+' DIAS',oFont1,50)			  	   		       			
			oPrint:Say(xLinha+10,2330,cValToChar(aDados3[nX,06])+' DIAS',oFont1,50)		   	
			oPrint:Say(xLinha+10,2750,cValToChar(aDados3[nX,08])+' DIAS',oFont1,50)		   						 
		Else		                                                    
			oPrint:Say(xLinha+10,1910,cValToChar(aDados3[nX,04]),oFont1,50)		   		       
			oPrint:Say(xLinha+10,2330,cValToChar(aDados3[nX,06]),oFont1,50) 
			oPrint:Say(xLinha+10,2750,cValToChar(aDados3[nX,08]),oFont1,50) 		 
		EndIf

		If nX == 1 .Or. nX == 5 .Or. nX == 6		       
			oPrint:Say(xLinha+10,3395,	Alltrim(Transform(aDados3[nX,10],"@E 99,999.99")),oFont1,,,,1) 
		Else
			oPrint:Say(xLinha+10,3210,cValToChar(aDados3[nX,10]),oFont1,50)		
		EndIf 
						
		xLinha+=50 		
			                                                
	Next nX	

	fRodape(@oPrint)
	 			
	oPrint:EndPage()
	oPrint:Preview()
	
Return()
                 

Static Function fCabecalho(oPrint,lProxPag)
*********************************************************************************
*
*
**** 
    If lProxPag
	 	xLinha := 80       
		oPrint:Box(xLinha,30,xLinha + 70,3400)
		oPrint:Say(xLinha+20,1600,OemToAnsi("MAPA DE COTAÇÃO"),oFont2,130)    
		xLinha+=70 
		oPrint:Box(xLinha,2980,xLinha + 50,3400)	// Box no lugar do formador de preço.    
		++nPag
	EndIf
			
	// Linha Título
	oPrint:Box(xLinha,0030,xLinha + 50,1720)      	// Itens de Compra       
	oPrint:Say(xLinha+10,0710,"ITENS DA COMPRA",oFont2,50)	
	oPrint:Box(xLinha,1720,xLinha + 50,2140)     	// Preço 1   
	oPrint:Say(xLinha+10,1880,"PREÇO 1",oFont2,50)		
	oPrint:Box(xLinha,2140,xLinha + 50,2560)		// Preço 2   
	oPrint:Say(xLinha+10,2300,"PREÇO 2",oFont2,50)			
	oPrint:Box(xLinha,2560,xLinha + 50,2980)		// Preço 3
	oPrint:Say(xLinha+10,2720,"PREÇO 3",oFont2,50)			
	xLinha+=50	
	
	// Linha Subtítulo       	 
	oPrint:Box(xLinha,0030,xLinha + 50,0085)    	// Item     
	oPrint:Say(xLinha+10,0043,"N°",oFont2,50)	
	oPrint:Box(xLinha,0085,xLinha + 50,0140)   		// Unidade
	oPrint:Say(xLinha+10,0095,"UM",oFont2,50)	
	oPrint:Box(xLinha,0140,xLinha + 50,0240)   		// Quantidade
	oPrint:Say(xLinha+10,0155,"Qtde",oFont2,50)	   
	oPrint:Box(xLinha,0240,xLinha + 50,0960)       	// Descrição    
	oPrint:Say(xLinha+10,0500,"Descrição",oFont2,50)	
	oPrint:Box(xLinha,0960,xLinha + 50,1720)      	// Observações  
	oPrint:Say(xLinha+10,1280,"Observações",oFont2,50)

	oPrint:Box(xLinha,1720,xLinha + 50,1900)        // Vlr.Unitário - Fornecedor 1    
	oPrint:Say(xLinha+10,1780,"Unit.",oFont2,50)	
	oPrint:Box(xLinha,1900,xLinha + 50,2140)		// Vlr.Total
	oPrint:Say(xLinha+10,1970,"Total",oFont2,50)	
	    	
	oPrint:Box(xLinha,2140,xLinha + 50,2320)        // Vlr.Unitário - Fornecedor 2  
	oPrint:Say(xLinha+10,2200,"Unit.",oFont2,50)	
	oPrint:Box(xLinha,2320,xLinha + 50,2560)		// Vlr.Total  
	oPrint:Say(xLinha+10,2390,"Total",oFont2,50)	 
			
	oPrint:Box(xLinha,2560,xLinha + 50,2740)        // Vlr.Unitário - Fornecedor 3  
	oPrint:Say(xLinha+10,2620,"Unit.",oFont2,50)	
	oPrint:Box(xLinha,2740,xLinha + 50,2980)		// Vlr.Total   	
	oPrint:Say(xLinha+10,2810,"Total",oFont2,50)	  
	
	oPrint:Box(xLinha,2980,xLinha + 50,3160)        // Vlr.Unitário - Formador de Preço 
	oPrint:Say(xLinha+10,3040,"Unit.",oFont2,50)	
	oPrint:Box(xLinha,3160,xLinha + 50,3400)		// Vlr.Total 	
	oPrint:Say(xLinha+10,3230,"Total",oFont2,50)		 
	xLinha+=50 
	
Return()


Static Function fRodape(oPrint)
*********************************************************************************
*
*
**** 
	xLinha+=50 
	oPrint:Say(xLinha+10,0030,"Cotação: "+cFilDe+"-"+cCotaDe,oFont1,50)
	oPrint:Say(xLinha+10,1700,"Pág. "+cValToChar(nPag),oFont1,50)
	oPrint:Say(xLinha+10,3020,"Impresso em: "+DtoC(Date()),oFont1,50)
		
Return()