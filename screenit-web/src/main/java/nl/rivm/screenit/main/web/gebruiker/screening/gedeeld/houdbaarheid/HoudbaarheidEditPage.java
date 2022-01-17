
package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.planning.web.component.DatePickerHelper;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.SubmitLink;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.validation.IFormValidator;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;

public abstract class HoudbaarheidEditPage<H extends AbstractHoudbaarheid> extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private HoudbaarheidService ifobtVervaldatumService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public HoudbaarheidEditPage(final IModel<H> model)
	{
		add(new Label("title", getTitleModel(model)));

		Form<H> form = new Form<>("form", new CompoundPropertyModel<>(model));
		add(form);

		final TextField<String> barcodeStart = new TextField<>("barcodeStart");
		final TextField<String> barcodeEnd = new TextField<>("barcodeEnd");
		customizeBarcodeFields(barcodeStart, barcodeEnd);
		Component type = createTypeField("type");

		form.add(barcodeStart.setRequired(true));
		form.add(barcodeEnd.setRequired(true));
		form.add(DatePickerHelper.newDatePicker("vervalDatum").setRequired(true));
		form.add(type);

		form.add(new IFormValidator()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void validate(Form<?> form)
			{
				if (barcodeStart.getConvertedInput().length() != barcodeEnd.getConvertedInput().length())
				{
					form.error(getString("error.lengthebarcode.ongelijk"));
				}
				if (barcodeEnd.getConvertedInput().compareTo(barcodeStart.getConvertedInput()) < 0)
				{
					form.error(getString("error.einbarcode.voor.beginbarcode"));
				}
			}

			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent<?>[] { barcodeStart, barcodeEnd };
			}
		});

		form.add(new SubmitLink("submit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit()
			{
				DateTime dt = new DateTime();
				H vervalDatum = model.getObject();
				if (ifobtVervaldatumService.overlaptBestaandeReeks(ModelProxyHelper.deproxy(vervalDatum)))
				{
					ScreenitSession.get().error(getString("error.overlappendereeks"));
				}
				else if (vervalDatum.getVervalDatum().after(currentDateSupplier.getDate()))
				{
					if (vervalDatum.getVervalDatum() != null && new DateTime(vervalDatum.getVervalDatum()).isAfter(dt.plusMonths(6)))
					{
						ScreenitSession.get().info("De batch is succesvol opgeslagen");
					}
					else
					{
						ScreenitSession.get().error("LET OP! De batch is succesvol opgeslagen maar de einddatum ligt binnen 6 maanden");
					}
					vervalDatum.setLengthBarcode(vervalDatum.getBarcodeStart().length());
					hibernateService.saveOrUpdate((AbstractHoudbaarheid) getForm().getModelObject());
					setResponsePage(getActiveSubMenuClass());
				}
				else
				{
					ScreenitSession.get().error(getString("error.datuminverleden"));
				}

			}

		});
		form.add(new BookmarkablePageLink<>("annuleren", getActiveSubMenuClass()));
	}

	protected void customizeBarcodeFields(TextField<String> barcodeStart, TextField<String> barcodeEnd)
	{

	}

	protected abstract Component createTypeField(String id);

	protected abstract IModel<String> getTitleModel(IModel<H> model);

}
