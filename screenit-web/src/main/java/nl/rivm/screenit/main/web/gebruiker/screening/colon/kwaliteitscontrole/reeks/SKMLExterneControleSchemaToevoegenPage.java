package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.util.Calendar;
import java.util.Date;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.web.component.DatePickerHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;
import org.wicketstuff.wiquery.ui.datepicker.DatePickerYearRange;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class SKMLExterneControleSchemaToevoegenPage extends KwaliteitscontroleBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	public SKMLExterneControleSchemaToevoegenPage(IModel<SKMLExternSchema> schemaModel)
	{
		setDefaultModel(schemaModel);

		add(new Link<Void>("terugViaTitle")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(SKMLExterneControleSchemaPage.class);
			}
		});

		ScreenitForm<SKMLExternSchema> schemaForm = new ScreenitForm<SKMLExternSchema>("schemaForm", getModel());
		schemaForm.add(new TextField<>("jaar", Integer.class).setRequired(true));
		schemaForm.add(new TextField<>("ronde", Integer.class).setRequired(true));
		TextField<String> letter = new TextField<>("letter", String.class);
		letter.add(new StringValidator(1, 255));
		letter.setRequired(true);
		letter.setOutputMarkupId(true);
		schemaForm.add(letter);

		DatePicker<Date> deadline = DatePickerHelper.newDatePicker("deadline")
			.setYearRange(new DatePickerYearRange((short) -80, Short.parseShort(Integer.toString(Calendar.getInstance().get(Calendar.YEAR) + 10)))).setChangeMonth(true);
		deadline.setRequired(true);
		deadline.setOutputMarkupId(true);
		schemaForm.add(deadline);

		schemaForm.add(new Link<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(SKMLExterneControleSchemaPage.class);
			}
		});
		schemaForm.add(new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				SKMLExternSchema schema = (SKMLExternSchema) getForm().getModelObject();
				schema.setActief(true);
				hibernateService.saveOrUpdate(schema);
				getSession().info("Schema succesvol opgeslagen");
				setResponsePage(SKMLExterneControleSchemaPage.class);
			};
		});
		add(schemaForm);
	}

	@SuppressWarnings("unchecked")
	private IModel<SKMLExternSchema> getModel()
	{
		return (IModel<SKMLExternSchema>) getDefaultModel();
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return SKMLExterneControleSchemaPage.class;
	}
}
