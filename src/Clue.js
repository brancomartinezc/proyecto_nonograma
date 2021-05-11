import React from 'react';

class Clue extends React.Component {
    render() {
        const clue = this.props.clue;
        const sat = this.props.sat;
        return (
            <div className={"clue" + (sat ? " satisfies" : "")} >
                {clue.map((num, i) =>
                    <div key={i}>
                        {num}
                    </div>
                )}
            </div>
        );
    }
}

export default Clue;