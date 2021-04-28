import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        const numOfRows = this.props.grid.length;
        const numOfCols = this.props.grid[0].length;
        //console.log(this.props.class_pista);

        //pistas filas y columnas de testeo
        return (
            <div>

                <div className="board"
                
                 style={{
                    gridTemplateRows: 'repeat(' + 2 + ', 40px)',
                    gridTemplateColumns: 'repeat(' + numOfCols + ', 40px)'
                }}>
                    {this.props.pistas_columnas.map((row, i) =>
                        row.map((cell, j) =>
                            <Square
                                value={cell}
                                key={70+i+j}
                            />
                        )
                    )}
                </div>
                <div style={{display: 'inline-block'}}>
                    <div className="board"
                    style={{
                        gridTemplateRows: 'repeat(' + numOfRows + ', 40px)',
                        gridTemplateColumns: 'repeat(' + 2 + ', 40px)'
                    }}>
                        {this.props.pistas_filas.map((row, i) =>
                        row.map((cell, j) =>
                            <Square
                                value={cell}
                                key={70+i+j}
                            />
                        )
                    )}
                    </div>
    
                    <div className="board" 
                     style={{
                        gridTemplateRows: 'repeat(' + numOfRows + ', 40px)',
                        gridTemplateColumns: 'repeat(' + numOfCols + ', 40px)'
                    }}>
                    {this.props.grid.map((row, i) =>
                        row.map((cell, j) =>
                            <Square
                                value={cell}
                                onClick={() => this.props.onClick(i, j)}
                                key={i + j}
                            />
                        )
                    )}
                    </div>
                </div>

            </div>
            
        );
    }
}

export default Board;