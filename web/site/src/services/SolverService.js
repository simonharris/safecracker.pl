const API_HOST = 'http://127.0.0.1:5000'


class SolverService {
    constructor() {
        this.eventSource = null;
        this.listeners = [];
        this.messages = [];
    }

    addListener(listener) {
        this.listeners.push(listener);
    }

//   removeListener(listener) {
//     const index = this.listeners.indexOf(listener);
//     if (index !== -1) {
//       this.listeners.splice(index, 1);
//     }
//   }

    solve(puzzleid) {
        //alert('GOT PUZZ:' + puzzleid);

        this.eventSource = new EventSource(API_HOST + '/solve/' + puzzleid);

        this.eventSource.onerror = () => {
            console.log('Error occurred. Ready state:', this.eventSource.readyState);
        };
        this.eventSource.onopen = () => {
            console.log('Connection established');
        };
        this.eventSource.addEventListener('begin', (event) => {
            console.log('Received "begin" event:', event.data);
            this.messages = [];
            this.messages.push(event.data);
        });
        this.eventSource.addEventListener('end', (event) => {
            console.log('Received "end" event:', event.data);
            this.eventSource.close();
            this.messages.push(event.data);
            //console.log(this.messages);
        });
        this.eventSource.addEventListener('message', (event) => {
            console.log('Received "message" event:', event.data);
            this.messages.push(event.data);
        });
        this.eventSource.addEventListener('solution', (event) => {
            console.log('Received "solution" event:', event.data);
        });
    }

    async getExamples() {
        try {
            const response = await fetch(API_HOST + '/examples');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            return data;
        } catch (error) {
            console.error(error);
            throw error;
        }
    }
}

export default SolverService;