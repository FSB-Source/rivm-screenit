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
import {FormFeedback, Input} from 'reactstrap';
import type {Form, FORM_FIELD_ID, FormField} from '../../datatypes/Form';
import {getMandatory} from '../../util/MapUtil';
import type {Validation} from '../../validation/Validation';

export type ValidationInputValueProps<T> = {
    value: string;
    label: string;
    transformValue?: (string) => T,
    fieldId: FORM_FIELD_ID;
    form: Form;
    maxLength: ?number;
    disabled: boolean;
    placeholder: ?string;
    type?: string;
    color?: string;
    className?: string;
    validator: Validation<T>;
    showRedErrorBackground?: boolean,

    updateField: (value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: ?boolean) => void;
    onChange: (inputValue: string) => void;
}

export default class ValidationInputValue<T> extends React.Component<ValidationInputValueProps<T>> {

    constructor(props: ValidationInputValueProps<T>) {
        super(props);
        this.props = props;
        this.updateValue = this.updateValue.bind(this);
        this.onBlur = this.onBlur.bind(this);
    }

    updateValue = (event: Event) => {
        let target = event.target;
        if (target instanceof HTMLInputElement || target instanceof HTMLTextAreaElement) {
            this.props.onChange(target.value);
            this.props.updateField(this.getValidationValue(target.value), this.props.fieldId, this.props.form);
        }
    };

    getCurrentField(): FormField<?T> {
        return getMandatory(this.props.form.fieldsById, this.props.fieldId);
    }

    getValidationValue(value: string): T {
        return this.props.transformValue ? this.props.transformValue(value) : (value: any);
    }

    onBlur = () => {
        this.props.updateField(this.getValidationValue(this.props.value), this.props.fieldId, this.props.form, true);
    };

    render() {
        return (
            <div>
                <Input id={this.getCurrentField().label}
                       type={this.props.type}
                       placeholder={this.props.placeholder || ''}
                       disabled={this.props.disabled}
                       value={this.props.value}
                       invalid={this.getCurrentField().showError ? !this.getCurrentField().isValid : false}
                       onChange={this.updateValue}
                       maxLength={this.props.maxLength}
                       onBlur={this.onBlur}
                       className={this.props.className}
                       style={{backgroundColor: this.props.color}}
                />
                {this.props.showRedErrorBackground ?
                    <FormFeedback className={'error-onderzoekscherm'}>{this.getCurrentField().errorMessage}</FormFeedback> :
                    <FormFeedback>{this.getCurrentField().errorMessage}</FormFeedback>
                }
            </div>
        );
    }
}
