import Shepherd from 'shepherd.js';

import type { RoundTour } from '../interfaces';

export function initModule(): RoundTour {
  return {
    corresRematchOffline,
  };

  function corresRematchOffline() {
    const tour = new Shepherd.Tour();

    tour.addStep({
      title: 'Вызов на реванш отправлен',
      text: 'Соперник сейчас не в сети, но сможет принять вызов позже!',
      attachTo: {
        element: 'button.rematch',
        on: 'bottom',
      },
      buttons: [
        {
          action() {
            return this.next();
          },
          text: 'Понятно',
        },
      ],
    });

    tour.start();
  }
}
