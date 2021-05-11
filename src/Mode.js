import React from 'react';

class Mode extends React.Component {
    render() {
        return (
            <button className="square" onClick={this.props.onClick} disabled={this.props.gameWon}>
                {this.props.value}
            </button>
        );
    }
}

export default Mode;