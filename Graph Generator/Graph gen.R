graph=function(fname,col=NULL,direc='D:\\DS\\Machine learning\\Datasets',comp=NULL,targ=NULL) #Main function- fname is file name, col takes vector
{                                                                                             #entry for the columns whose graphs are required,  
  {                                                                                           #direc is directory path, comp is the comparison column    
    if(!dir.exists(direc)) #Checking if the specified directory exists or no                  #targ is the target column
    {
      stop('Directory doesnt exist')
    }
    else
    {
      setwd(direc) #If directory exists we will set it as working directory
    }
  }
  if(!file.exists(fname))
  {
    stop('File doesnt exist') #Check if file exists in the directory
  }
  df=read.csv(fname) #Reading the file
  if(!is.data.frame(df))
  {
    stop('Not a data frame') #Checking if the imported file is a dataframe or no
  }
  if(is.null(col))
  {
    col=names(df) #Setting all columns if no specific columns were specified
  }
  if(!dir.exists(substr(fname,1,rev((gregexpr('.',fname,fixed=T))[[1]])[1]-1))) #Checking if a directory exists with name of dataset
  {
    dir.create(substr(fname,1,rev((gregexpr('.',fname,fixed=T))[[1]])[1]-1)) #Creating directory if it doesnt exist
  }
  setwd(paste(direc,substr(fname,1,rev((gregexpr('.',fname,fixed=T))[[1]])[1]-1),sep='\\')) #Changing working directory to dataset directory
  for (i in col)
  {
    {
      if((is.numeric(df[,i])) & (length(table(df[,i]))>10)) # Checking if column is numeric and has enough unique values
      {
        png(paste(i,'.png',sep='')) 
        par(mfrow=c(2,1)) 
        boxplot(df[,i],main=paste('Boxplot of',i),ylab=i,col='maroon',border='grey5',horizontal=T)
        hist(df[,i],main=paste('Histogram of',i),xlab=i,col='orange',border='grey5')
        dev.off()
      }
      else # Non numeric columns and numeric columns with very few unique values
      {
        if(length(table(df[,i]))<=10) #Limiting to only columns with less than 10 unique values else graph wont be readable
          {
          png(paste(i,'.png',sep=''))
          barplot(table(df[,i]),main=paste('Frequency distribution of',i),xlab=i,col='lightgreen',border='grey5')
          dev.off()
          }
      }
    }
    if((!is.null(comp)) & (is.numeric(df[,i])) & (!is.numeric(df[,comp]))) #Graphs to plot if comparison argument is present
    {
      png(paste(i,' by ',comp,'.png',sep=''))
      boxplot(df[,i]~df[,comp],main=paste('Side by side boxplot of ',i,' w.r.t. ',comp,sep=''),xlab=i,ylab=comp,col='lightblue',border='grey5',horizontal=T)
      dev.off()
    }
    if((!is.null(targ)) & (targ!=i) & (sum(targ==names(df))>0)) #Graphs to plot if target argument is present
    {
      if((is.numeric(df[,i])) & (is.numeric(df[,targ]))&(length(table(df[,targ]))>10)) #Checking if both target and ith column are numeric and whether enough unique values are there
      {
        png(paste('Scatter plot for ',i,' by ',targ,'.png',sep=''))
        plot(df[,i],df[,targ],type='p',xlab=i,ylab=targ,main=paste('Scatter plot for',targ,'vs',i),col='blue')
        dev.off()
      }
      if((!is.numeric(df[,i])) & ((!is.numeric(df[,targ]))|(length(table(df[,targ]))<10))) #Checking if crosstab is suitable
      {
        png(paste('Crosstab for ',i,' by ',targ,'.png',sep=''))
        barplot(table(df[,targ],df[,i]),legend=rownames(table(df[,targ],df[,i])),border='grey5',xlab=i,ylab='Counts',beside=T)
        dev.off()
      }
    }
  }
}
graph('cars.csv',comp='Origin',targ='MPG')
graph('mushroom.csv',targ='class')
graph('attrition.csv',targ='Attrition')
graph('concrete.csv',targ='CompStrength')
