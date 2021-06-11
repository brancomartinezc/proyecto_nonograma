import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Mode from './Mode';

class Game extends React.Component {

  pengine;
  
  constructor(props) {
    super(props);
    this.state = {
      grid: null,
      rowClues: null,
      colClues: null,
      waiting: false,
      grillaEnJuego: null,
      grillaResuelta: null,
      estadoRevelandoCelda: false,
      estadoRevelandoTablero: false,
      mode: "#",
      filasCorrectas: [],
      colsCorrectas: [],
      gameWon: false,
      statusText: "Juego en progreso."
    };

    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
    this.modeClick = this.modeClick.bind(this);
  }

  handlePengineCreate() {
    const queryS = 'init(PistasFilas, PistasColumns, Grilla)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grilla'],
          rowClues: response['PistasFilas'],
          colClues: response['PistasColumns'],
        });
        
        //inicializacion de arreglos de control de filas y columnas correctas.
        this.state.grid.forEach(() => {
          this.state.filasCorrectas.push(0);
        });

        this.state.grid[0].forEach(() => {
          this.state.colsCorrectas.push(0);
        });

        this.obtenerSolucion();
      }
    });
  }

  //query a prolog para obtener la solucion de la grilla
  obtenerSolucion(){
    const filas = JSON.stringify(this.state.rowClues);
    const columnas = JSON.stringify(this.state.colClues);
    const queryS = `solve(${filas}, ${columnas}, Solucion)`;

    this.pengine.query(queryS, (succes, response) => {
      if(succes){
        this.setState({
          grillaResuelta: response['Solucion']
        });
      }
    });
  }
  
  handleClick(i, j) {
    if (this.state.waiting || this.state.estadoRevelandoTablero) {
      return;
    }
    
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const filas = JSON.stringify(this.state.rowClues);
    const columnas = JSON.stringify(this.state.colClues);
    
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    // si esta revelando celda se pinta lo que coresponde, sino, se pinta lo que el jugador eligio
    const queryS = !this.state.estadoRevelandoCelda ? 
    `put("${this.state.mode}", [${i}, ${j}], ${filas}, ${columnas}, ${squaresS}, GrillaRes, FilaSat, ColSat)` : 
    `put("${this.state.grillaResuelta[i][j]}", [${i}, ${j}], ${filas}, ${columnas}, ${squaresS}, GrillaRes, FilaSat, ColSat)`;
    //console.log(queryS); //DEBUG

    this.setState({
      waiting: true,
    });
    this.pengine.query(queryS, (success, response) => {
      
      let auxFilas = this.state.filasCorrectas.slice();
      auxFilas[i] = response['FilaSat'];
      let auxCols = this.state.colsCorrectas.slice();
      auxCols[j] = response['ColSat'];

      if (success) {
        this.setState({
          grid: response['GrillaRes'],
          filasCorrectas: auxFilas,
          colsCorrectas: auxCols,
          waiting: false
        });
        
        //control de victoria, si todas las filas y columnas son correctas gano.
        let todasFilasCorrectas = this.state.filasCorrectas.every(elem => elem === 1);
        let todasColsCorrectas = this.state.colsCorrectas.every(elem  => elem === 1);
        
        if(todasColsCorrectas && todasFilasCorrectas){
          this.setState({
            gameWon: true,
            statusText: "Ganaste!"
          }) 
        }

      } else {
        this.setState({
          waiting: false
        });
      }
    });
    
  }

  modeClick(){
    if (this.state.mode === "#"){
      this.setState({ mode: "X" });
    } else{
      this.setState({ mode: "#" });
    }
  }

  revelarTablero() {
    if(!this.state.estadoRevelandoTablero){
      this.setState({ 
        estadoRevelandoTablero: true,
        grillaEnJuego: this.state.grid, //guarda la grilla del jugador
        grid: this.state.grillaResuelta
      });  
    }else{
      this.setState({ 
        estadoRevelandoTablero: false,
        grid: this.state.grillaEnJuego
      });
    }
  }

  revelarCelda() {
    if(!this.state.estadoRevelandoCelda){
      this.setState({ 
        estadoRevelandoCelda: true,
        statusText: "Revelando celdas."
      });
    }else{
      this.setState({ 
        estadoRevelandoCelda: false,
        statusText: "Juego en progreso."
      });
    }
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    
    return (
      <div className="game">
        <h1>Nonograma</h1>
        <div>
          Modo actual: <Mode value={this.state.mode} class={"square"} gameWon={this.state.gameWon} onClick={() => this.modeClick()}/>
        </div>

        {<div className="gameInfo">
          {this.state.statusText}
        </div>}

        <Board
          grid={this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          gameWon={this.state.gameWon}
          filasCorrectas={this.state.filasCorrectas}
          columnasCorrectas={this.state.colsCorrectas}
          onClick={(i, j) => this.handleClick(i,j)}
        />

        <div>
          <Mode value={"🔍 Revelar celda"} class={"revelarCelda"} gameWon={this.state.gameWon} onClick={() => this.revelarCelda()}/>
        </div>

        <div>
          <Mode value={"🏁 Revelar tablero"} class={"revelarTablero"} gameWon={this.state.gameWon} onClick={() => this.revelarTablero()}/>
        </div>
      </div>
    );
  }
}

export default Game;
