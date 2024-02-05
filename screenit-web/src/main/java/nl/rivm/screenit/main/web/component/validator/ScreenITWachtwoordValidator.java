package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.service.WachtwoordService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.password.web.component.WachtwoordValidator;

import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.ValidationError;

public class ScreenITWachtwoordValidator extends WachtwoordValidator
{

	@SpringBean
	private WachtwoordService wachtwoordService;

	private final IModel<Gebruiker> gebruikerIModel;

	public ScreenITWachtwoordValidator(FormComponent<String> usernameField, boolean verplicht, IModel<Gebruiker> gebruikerIModel)
	{
		super(usernameField, verplicht);
		this.gebruikerIModel = gebruikerIModel;
	}

	@Override
	public void validate(IValidatable<String> validatable)
	{
		super.validate(validatable);

		Gebruiker gebruiker = ModelUtil.nullSafeGet(gebruikerIModel);
		if (gebruiker != null && wachtwoordService.isEerderGebruiktWachtwoord(gebruiker, validatable.getValue(),
			wachtwoordService.getVorigeWachtwoorden(gebruiker, LocalDate.now().minusYears(2))))
		{
			ValidationError error = new ValidationError();
			error.addKey("password_used_in_past");
			validatable.error(error);
		}
	}
}
