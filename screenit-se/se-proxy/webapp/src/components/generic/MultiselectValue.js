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
import Select from 'react-select';

export type MultiselectProps<T> = {
    id: string,
    options: Array<{ value: T, label: string }>,
    disabled?: boolean,
    handleChange: (selectedOptions: Array<T>) => void,
    value: Array<{ value: T, label: string }>
}

export default class MultiselectValue<T> extends Component<MultiselectProps<T>> {

    constructor(props: MultiselectProps<T>) {
        super(props);
        this.props = props;
        this.handleChange = this.handleChange.bind(this);
    }

    handleChange = (selectedOptions: ?Array<{ value: T, label: string }>): void => {
        this.props.handleChange(selectedOptions ? selectedOptions.map(v => v.value) : []);
    };

    render() {
        return (
            <Select className={'select-container-lavendel multiselect-container'} id={this.props.id} style={{backgroundColor: 'lavender', color: '#000!important'}}
                    placeholder={'Kies...'}
                    value={this.props.value}
                    onChange={this.handleChange}
                    options={this.props.options}
                    isDisabled={this.props.disabled}
                    isMulti={true}
                    isClearable={false}
                    noOptionsMessage={() => 'Geen resultaten'}>
            </Select>
        );

    }

}
