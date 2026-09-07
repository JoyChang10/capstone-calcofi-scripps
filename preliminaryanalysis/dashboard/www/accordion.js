// www/accordion.js

function updatePills(mode) {
  document.getElementById('pill-species').className = 'mode-pill' + (mode === 'species' ? ' mode-pill-active' : '');
  document.getElementById('pill-habitat').className = 'mode-pill' + (mode === 'habitat' ? ' mode-pill-active' : '');
}

function applyFilterMode(mode) {
  if (mode === 'habitat') {
    $('#species_panel').hide();
    $('#habitat_panel').show();
    var sub = $('input[name="habitat_submode"]:checked').val() || 'habitat_type';
    applyHabitatSubmode(sub);
  } else {
    $('#species_panel').show();
    $('#habitat_panel').hide();
  }
}

function applyHabitatSubmode(submode) {
  if (submode === 'habitat_type') { $('#habitat_type_panel').show(); $('#grpname_panel').hide(); }
  else                            { $('#habitat_type_panel').hide(); $('#grpname_panel').show(); }
}

$(document).on('shiny:connected', function() {
  $('#habitat_panel').hide();
  $('#habitat_type_panel').show();
  $('#grpname_panel').hide();
});

$(document).on('shiny:inputchanged', function(e) {
  if (e.name === 'filter_mode')     applyFilterMode(e.value);
  if (e.name === 'habitat_submode') applyHabitatSubmode(e.value);
});
