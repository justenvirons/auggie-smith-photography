// Event delegation — works after renderUI re-renders the page
$(document).on('click', '.cms-tab', function () {
  var tab = $(this).data('tab');
  $('.cms-tab').removeClass('active');
  $('.cms-panel').removeClass('active');
  $(this).addClass('active');
  $('#tab-' + tab).addClass('active');
  // Force DataTables to recalculate after the manage panel becomes visible
  if (tab === 'manage') {
    setTimeout(function () { $(window).trigger('resize'); }, 100);
  }
});
