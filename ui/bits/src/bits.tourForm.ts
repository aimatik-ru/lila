import flatpickr from 'flatpickr';

import { use24h } from 'lib/i18n';

site.load.then(() => {
  // the staff form (/tournament/manager) prefixes its fields with "setup."
  const $variant = $('#form3-variant, #form3-setup_variant');
  const showPosition = () =>
    $('.form3 .position').toggleClass('none', !['1', 'standard'].includes($variant.val() as string));

  $variant.on('change', showPosition);
  showPosition();

  $('.flatpickr').each(function (this: HTMLInputElement) {
    flatpickr(this, {
      minDate: 'today',
      maxDate: new Date(Date.now() + 1000 * 3600 * 24 * 366 * 2),
      dateFormat: 'Z',
      altInput: true,
      altFormat: 'Y-m-d h:i K',
      monthSelectorType: 'static',
      disableMobile: true,
      time_24hr: use24h(),
    });
  });
});
