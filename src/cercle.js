function workingJudge(inStart){
  if (inStart) {
    if(count+1 < timelist[0]){
      return true;
    }else if(count < timelist[0] + interval){
      return false;
    }else {
      return true;
    }
  }
}

function updateChart(chart,inStart){
    var workingFlag = workingJudge(inStart);

    if(count < (timelist[0]+timelist[1])){
      count += 1;
    }else{
      count = 0;
    }

    if(count = 0){
      timeKeeper = [timelist[0],0,timelist[1],0]
    }else if(workingFlag){
      timeKeeper[0] -= 1;
      timeKeeper[1] += 1;
    }else{
      dt = chart.data.data
      timeKeeper[2] -= 1;
      timeKeeper[3] -= 1;
    }

    chart.update();
  }
