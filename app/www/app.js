/* ---------------------------------------------------------------------
  navi
--------------------------------------------------------------------- */
  function toggleMobileNav() {
    var nav = document.getElementById('main-navigation-toggle');
    nav.classList.toggle('show');
  }
function toggleDropdown(event, id) {
  event.preventDefault();
  var menu = document.getElementById(id);
  var isOpen = menu.classList.contains('show');
  // close any other open dropdown first
  document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function (m) {
    m.classList.remove('show');
  });
  if (!isOpen) menu.classList.add('show');
}
// click outside the nav closes any open dropdown
document.addEventListener('click', function (e) {
  if (!e.target.closest('.navbar')) {
    document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function (m) {
      m.classList.remove('show');
    });
  }
});
/* ---------------------------------------------------------------------
  sidebar toggle - need to resize map
--------------------------------------------------------------------- */
  function toggleSidebar(side) {
    var wrapper = document.getElementById(side + '-sidebar');
    wrapper.classList.toggle('open');
    setTimeout(function () {
      if (window.mapInvalidate) window.mapInvalidate();
    }, 320);
  }
function closeSidebars() {
  document.getElementById('left-sidebar').classList.remove('open');
  document.getElementById('right-sidebar').classList.remove('open');
}
window.mapInvalidate = function () {
  var widget = HTMLWidgets.find('#map');
  if (widget && widget.getMap) { widget.getMap().invalidateSize(); }
};
window.addEventListener('resize', function () {
  if (window.mapInvalidate) window.mapInvalidate();
});
document.addEventListener('DOMContentLoaded', function () {
  // start collapsed on mobile so the map isn't blocked on first paint
  if (window.innerWidth <= 768) {
    closeSidebars();
  }
  var overlay = document.getElementById('mobile-overlay');
  if (overlay) overlay.addEventListener('click', closeSidebars);
});

/* ---------------------------------------------------------------------
  Shiny -> JS bridge: open right sidebar when server sets active dataset
--------------------------------------------------------------------- */
if (window.Shiny) {
  Shiny.addCustomMessageHandler('openRightSidebar', function (msg) {
    var wrapper = document.getElementById('right-sidebar');
    if (wrapper && !wrapper.classList.contains('open')) {
      toggleSidebar('right');
    }
  });
}