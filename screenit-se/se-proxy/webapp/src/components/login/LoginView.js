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

import React, {Component} from 'react';
import {Spinner} from 'reactstrap';
import {login} from '../../restclient/AuthenticatieRestclient';
import PropTypes from 'prop-types';
import {readNFC} from '../../restclient/NfcRestClient';
import {dismissAllToasts, persistentErrorToast} from '../../util/ToastUtil';
import {nuTimestamp} from '../../util/DateUtil';

export type LoginProps = {
    dubbeleInstantie: boolean;
    inlogActief: boolean;
}

export default class LoginView extends Component<LoginProps, any> {

    handleChange = (event: any) => {
        this.setState({
            [event.target.name]: event.target.value,
        });
    };

    handleSubmit = (event: any) => {
        event.preventDefault();
        const username = this.state.username.trim();
        const password = this.state.password;
        if (username.length === 0) {
            persistentErrorToast('Gebruikersnaam is niet ingevuld.');
        } else if (password.length === 0) {
            persistentErrorToast('Wachtwoord is niet ingevuld.');
        } else {
            dismissAllToasts();
            console.log(nuTimestamp() + ': Gedrukt op inloggen');
            readNFC().then(function(responseData) {
                const nfcTag = responseData.public_id;
                const yubikey = responseData.otp;
                login(username, password, nfcTag, yubikey);
            }).catch(function(error) {
                console.error(nuTimestamp() + ': Fout tijdens inloggen: ' + error);
                persistentErrorToast('Fout tijdens inloggen');
            });
        }
    };

    constructor(props: PropTypes) {
        super(props);
        this.state = {
            username: '',
            password: '',
        };
        this.handleChange = this.handleChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    render() {
        return (
            <form className="login" onSubmit={this.handleSubmit}>
                <Spinner className={this.props.inlogActief ? 'login-spinner' : 'login-spinner-disabled'}/>
                {this.props.inlogActief ? <div className={'laad-block-grijs'}/> : null}
                <div className="form-group row no-gutters">
                    <div className="offset-3 col-2">
                        <label htmlFor="gebruikersnaam" className="col-11 col-form-label text-right">Gebruikersnaam</label>
                    </div>
                    <div className="col-2">
                        <input id="gebruikersnaam" autoComplete="off" className="" type="text" name="username" onChange={this.handleChange} autoFocus
                               disabled={this.props.dubbeleInstantie}/>
                    </div>
                </div>
                <div className="form-group row no-gutters">
                    <div className="offset-3 col-2">
                        <label htmlFor="wachtwoord" className="col-11 col-form-label text-right">Wachtwoord</label>
                    </div>
                    <div className="col-2">
                        <input id="wachtwoord" type="password" name="password" autoComplete="off" onChange={this.handleChange} disabled={this.props.dubbeleInstantie}/>
                    </div>
                </div>
                <div className="form-group row no-gutters">
                    <div className="offset-5 col-2">
                        <input className={this.props.dubbeleInstantie || this.props.inlogActief ? 'btn-primary-se disabled' : 'btn btn-success'} type="submit" value="Inloggen"
                               disabled={this.props.dubbeleInstantie || this.props.inlogActief}
                        />
                    </div>
                </div>
            </form>
        );
    }
}
