document.getElementsByClassName('vertex').forEach(v => {
  v.onmouseover = () => v.classList.add('hover');
  v.onmouseout = () => v.classList.remove('hover');
});
