function FMComboBox_ApplyChange(id) {
	var sIndex=document.getElementById(id).selectedIndex;
	var sStr=document.getElementById(id).options[sIndex].text;
  document.getElementById(id+"EDITOR").value=sStr;
  return(true);
}