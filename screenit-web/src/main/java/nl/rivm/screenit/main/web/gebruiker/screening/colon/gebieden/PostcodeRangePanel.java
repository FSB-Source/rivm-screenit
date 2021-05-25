
package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.topicuszorg.wicket.input.converters.PostcodeConverter;
import nl.topicuszorg.wicket.input.validator.PostcodeValidator;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.convert.IConverter;

public class PostcodeRangePanel extends GenericPanel<UitnodigingsGebied>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonUitnodigingsgebiedService uitnodigingsGebiedService;

	public PostcodeRangePanel(String id, IModel<UitnodigingsGebied> model, Form<UitnodigingsGebied> form)
	{
		super(id, model);
		TextField<String> vanPostcode = new TextField<String>("postcodeGebied.vanPostcode")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (type == String.class)
				{
					return (IConverter<C>) new PostcodeConverter();
				}
				return super.getConverter(type);
			}

		};
		vanPostcode.add(new PostcodeValidator<>()).setRequired(true);
		add(vanPostcode);

		TextField<String> totPostcode = new TextField<String>("postcodeGebied.totPostcode")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (type == String.class)
				{
					return (IConverter<C>) new PostcodeConverter();
				}
				return super.getConverter(type);
			}

		};
		totPostcode.add(new PostcodeValidator<>()).setRequired(true);
		add(totPostcode);

		form.add(new PostcodeOverlapValidator(vanPostcode, totPostcode));
	}

	private class PostcodeOverlapValidator extends AbstractFormValidator
	{

		private static final long serialVersionUID = 1L;

		private final FormComponent<String> vanPostcode;

		private final FormComponent<String> totPostcode;

		public PostcodeOverlapValidator(FormComponent<String> vanPostcode, FormComponent<String> totPostcode)
		{
			this.vanPostcode = vanPostcode;
			this.totPostcode = totPostcode;
		}

		@Override
		public FormComponent<?>[] getDependentFormComponents()
		{
			return new FormComponent[] { vanPostcode, totPostcode };
		}

		@Override
		public void validate(Form<?> form)
		{
			PostcodeGebied postcodeGebied = getModelObject().getPostcodeGebied();
			if (postcodeGebied == null)
			{
				postcodeGebied = new PostcodeGebied();
			}

			int result = vanPostcode.getConvertedInput().compareTo(totPostcode.getConvertedInput());
			if (result > 0)
			{
				error(vanPostcode, "vanIsGroter");
			}

			postcodeGebied.setVanPostcode(vanPostcode.getConvertedInput());
			postcodeGebied.setTotPostcode(totPostcode.getConvertedInput());
			if (uitnodigingsGebiedService.findOverlappendePostcodeGebieden(postcodeGebied).size() > 0)
			{
				error(vanPostcode);
			}
		}

	}

}
