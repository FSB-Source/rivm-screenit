/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React from 'react';
import {store} from '../../Store';
import {createActionNavigateToDaglijst} from '../../actions/NavigationActions';
import type {ErrorMsg} from '../../restclient/ErrorRestClient';
import {verstuurError} from '../../restclient/ErrorRestClient';

type ErrorBoundaryProps = {
    children: any;
}

type ErrorBoundaryState = {
    hasError: boolean;
}

export default class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
    constructor(props: ErrorBoundaryProps) {
        super(props);
        this.state = {hasError: false};
    }

    componentDidCatch(error: Error, info: { componentStack: string }) {
        const errorMsg: ErrorMsg = {
            error: error,
            info: info,
        };
        this.setState({hasError: true});
    }

    render() {
        if (this.state.hasError) {
            store.dispatch(createActionNavigateToDaglijst());
        }
        return this.props.children;
    }
};

(function() {

    const oldConsole: any = console;

    const oldTrace = oldConsole.trace;
    oldConsole.trace = function(message) {
        verstuurError('trace: ' + message);
        oldTrace.apply(oldConsole, arguments);
    };

    const oldLog = oldConsole.log;
    oldConsole.log = function(message) {
        verstuurError('log: ' + message);
        oldLog.apply(oldConsole, arguments);
    };

    const oldWarn = oldConsole.warn;
    oldConsole.warn = function(message) {
        verstuurError('warn: ' + message);
        oldWarn.apply(oldConsole, arguments);
    };

    const oldError = oldConsole.error;
    oldConsole.error = function(message) {
        verstuurError('error: ' + message);
        oldError.apply(oldConsole, arguments);
    };

    window.addEventListener('error', function(e) {
        console.error('Error occurred: ' + e.error.stack);
    });

    window.addEventListener('unhandledrejection', function(e) {
        console.error('Unhandled rejection: ' + e.reason.stack);
    });

})();
