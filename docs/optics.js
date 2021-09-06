function ancestors(v) {
  return v.getAttribute("data-ancestors")?.split(' ').map(i => document.getElementById(i)) || [];
}
document.querySelectorAll('.vertex').forEach(v => {
  v.onmouseover = () => [v, ...ancestors(v)].forEach(e => e.classList.add('hover'));
  v.onmouseout = () => [v, ...ancestors(v)].forEach(e => e.classList.remove('hover'));
});
