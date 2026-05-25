// www/accordion.js

var YEAR_MIN_DEFAULT = 1951;
var YEAR_MAX_DEFAULT = 2023;

function updatePills(mode) {
  document.getElementById('pill-species').className = 'mode-pill' + (mode === 'species' ? ' mode-pill-active' : '');
  document.getElementById('pill-habitat').className = 'mode-pill' + (mode === 'habitat' ? ' mode-pill-active' : '');
}

function sliderIsCustom() {
  var el = $('#year_range');
  if (!el.length) return false;
  var api = el.data('ionRangeSlider');
  if (!api) return false;
  return (api.result.from !== YEAR_MIN_DEFAULT || api.result.to !== YEAR_MAX_DEFAULT);
}

function applyEnso(checked) {
  if (checked) {
    $('#year_range_wrap').css({'opacity':'0.35','pointer-events':'none'});
  } else {
    $('#year_range_wrap').css({'opacity':'1','pointer-events':'auto'});
  }
}

function applySliderChange() {
  if (sliderIsCustom()) {
    $('#show_enso_tog').closest('.toggle-row').css({'opacity':'0.35','pointer-events':'none'});
    if ($('#show_enso_tog').is(':checked')) {
      $('#show_enso_tog').prop('checked', false);
      Shiny.setInputValue('show_enso', false);
    }
  } else {
    $('#show_enso_tog').closest('.toggle-row').css({'opacity':'1','pointer-events':'auto'});
  }
}

function applySeasons(checked) {
  if (checked) {
    $('#filter_mode_wrap').css({'opacity':'0.35','pointer-events':'none'});
  } else {
    $('#filter_mode_wrap').css({'opacity':'1','pointer-events':'auto'});
  }
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
  if (e.name === 'show_enso')       applyEnso(e.value);
  if (e.name === 'year_range')      applySliderChange();
  if (e.name === 'show_seasons')    applySeasons(e.value);
  if (e.name === 'filter_mode')     applyFilterMode(e.value);
  if (e.name === 'habitat_submode') applyHabitatSubmode(e.value);
});
