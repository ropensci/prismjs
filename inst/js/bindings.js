function prism_languages(){
  return Object.keys(Prism.languages);
}

function prism_highlight(input, lang){
  const dict = Prism.languages[lang];
  if(!dict)
    throw "Prism does not have this language: " + lang;
  return Prism.highlight(input, dict, lang)
}
