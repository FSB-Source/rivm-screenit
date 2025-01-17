package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatiePropertyModel;
import nl.topicuszorg.wicket.input.validator.ValueValidator;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;
import org.springframework.scheduling.support.CronSequenceGenerator;

public class TechnischBeheerSeParametersPanel extends BaseTechnischBeheerParametersPanel
{
	public TechnischBeheerSeParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, new ParameterisatiePropertyModel<>(model));
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		var sePingIntervalVeld = new TextField<>("internalMammaSePingInterval", Integer.class).setRequired(true).add(ValueValidator.minimum(500));
		var sePongTimeoutVeld = new TextField<>("internalMammaSePongTimeout", Integer.class).setRequired(true).add(ValueValidator.minimum(500));

		form.add(sePingIntervalVeld);
		form.add(sePongTimeoutVeld);
		form.add(new TextField<>("internalMammaSeInformatieOphalenCron", String.class).add((IValidator<String>) validatable ->
		{
			if (!CronSequenceGenerator.isValidExpression(validatable.getValue()))
			{
				validatable.error(new ValidationError("Invalide cron expressie"));
			}
		}).setRequired(true));

		form.add(geefPongIsGroterDanPingValidator(sePingIntervalVeld, sePongTimeoutVeld));

		return form;
	}

	private AbstractFormValidator geefPongIsGroterDanPingValidator(FormComponent<Integer> sePingIntervalVeld, FormComponent<Integer> sePongTimeoutVeld)
	{
		return new AbstractFormValidator()
		{
			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent[] { sePingIntervalVeld, sePongTimeoutVeld };
			}

			@Override
			public void validate(Form<?> form)
			{
				var sePingIntervalWaarde = sePingIntervalVeld.getConvertedInput();
				var sePongTimeoutWaarde = sePongTimeoutVeld.getConvertedInput();
				if (sePongTimeoutWaarde < (sePingIntervalWaarde + 100))
				{
					error(sePongTimeoutVeld, "pongTimingValidationError");
				}
			}
		};
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("socketInstellingenOpslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target);
				info("De SE socket instellingen zijn opgeslagen");
			}
		};
	}
}
