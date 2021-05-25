package nl.rivm.screenit.main.web.gebruiker.login;

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

import java.util.Date;
import java.util.Iterator;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateSearchService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;
import nl.topicuszorg.wicket.password.web.component.WachtwoordValidator;

import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.joda.time.DateTime;
import org.joda.time.Period;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PasswordChangePage extends LoginBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(PasswordChangePage.class);

	@SpringBean
	private HibernateSearchService hibernateSearchService;

	@SpringBean
	private LogService logService;

	private final String gebruikersnaam;

	private String code;

	private String ww1;

	private String ww2;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private GebruikersService gebruikersService;

	private IModel<Gebruiker> medewerker;

	public PasswordChangePage(PageParameters pageParameters)
	{
		ScreenitSession.get().replaceSession();
		code = pageParameters.get("code").toString();
		gebruikersnaam = pageParameters.get("user").toString();

		Gebruiker searchObject = new Gebruiker();
		searchObject.setWachtwoordChangeCode(code);

		if (hibernateSearchService.count(searchObject) == 1)
		{
			Iterator<Gebruiker> gebruikerIter = hibernateSearchService.search(searchObject, -1, -1, "id", true);
			medewerker = ModelUtil.sModel(gebruikerIter.next());

			if (ModelUtil.nullSafeGet(medewerker) != null)
			{
				if (!isValidCode())
				{
					error(getLocalizer().getString("error.code.niet.meer.geldig", this));
				}
			}
			else
			{
				error(getLocalizer().getString("error.password.change.wrong.username", this));
			}
		}
		else
		{
			error(getLocalizer().getString("error.code.incorrect", this));
		}

		final ScreenitForm<PasswordChangePage> form = new ScreenitForm<PasswordChangePage>("requestForm", new CompoundPropertyModel<PasswordChangePage>(this));
		final BookmarkablePageLink<Void> naarinlogpagina = new BookmarkablePageLink<Void>("naarinlogpagina", Application.get().getHomePage());
		AjaxButton opslaan = new AjaxButton("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Gebruiker gebruiker = ModelUtil.nullSafeGet(medewerker);
				if (!isValidCode())
				{
					error("Code is niet meer geldig. Neem contact op met de beheerder.");
				}
				else if (!ww1.equals(ww2))
				{
					error("De beide wachtwoorden zijn niet gelijk aan elkaar.");
				}
				else if (!gebruiker.getGebruikersnaam().equals(gebruikersnaam))
				{
					error("Gebruikersnaam is niet geldig");
				}
				else
				{
					try
					{

						gebruikersService.setWachtwoord(gebruiker, ww1);

						gebruiker.setDatumWachtwoordAanvraag(null);
						gebruiker.setWachtwoordChangeCode(null);

						hibernateService.saveOrUpdate(gebruiker);
						logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GEWIJZIGD, gebruiker);
						info("Wachtwoord is opgeslagen");
						naarinlogpagina.setVisible(Boolean.TRUE);
						setVisible(Boolean.FALSE);

						Iterator<?> iter = form.iterator();
						while (iter.hasNext())
						{
							Component component = (Component) iter.next();
							if (!(component instanceof BookmarkablePageLink))
							{
								component.setEnabled(Boolean.FALSE);
								target.add(component);
							}
						}

						target.add(naarinlogpagina);

						target.add(form);
					}
					catch (Exception e)
					{
						LOG.error(e.getMessage(), e);
					}
				}
			}
		};
		opslaan.setOutputMarkupId(true);
		form.add(opslaan);

		naarinlogpagina.setOutputMarkupId(true);
		naarinlogpagina.setVisible(Boolean.FALSE);
		form.add(naarinlogpagina);
		form.setDefaultButton(opslaan);
		add(form);
		Component gebruikersnaamTf = ComponentHelper.addTextField(form, "gebruikersnaam", true, 50, true).add(new FocusBehavior());

		ComponentHelper.addTextField(form, "code", true, 50, true);

		FormComponent<String> ww1Field = new PasswordTextField("ww1").setRequired(true).setLabel(Model.of("Wachtwoord"));
		ww1Field.add(StringValidator.minimumLength(8));
		ww1Field.add(new WachtwoordValidator((TextField<String>) gebruikersnaamTf, true));
		ww1Field.setOutputMarkupId(true);
		ComponentHelper.setAutocompleteOff(ww1Field);
		form.add(ww1Field);
		FormComponent<String> ww2Field = new PasswordTextField("ww2").setRequired(true).setLabel(Model.of("Wachtwoord (nogmaals)"));
		ComponentHelper.setAutocompleteOff(ww2Field);
		ww2Field.setOutputMarkupId(true);
		form.add(ww2Field);

	}

	private boolean isValidCode()
	{
		DateTime now = new DateTime(new Date());
		Gebruiker gebruiker = ModelUtil.nullSafeGet(medewerker);
		if (gebruiker != null)
		{
			DateTime aanvraagDatum = new DateTime(gebruiker.getDatumWachtwoordAanvraag());

			Period period = new Period(aanvraagDatum, now);
			return period.getDays() < 1;
		}
		else
		{
			return false;
		}

	}

	public String getCode()
	{
		return this.code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public String getWw1()
	{
		return this.ww1;
	}

	public void setWw1(String ww1)
	{
		this.ww1 = ww1;
	}

	public String getWw2()
	{
		return this.ww2;
	}

	public void setWw2(String ww2)
	{
		this.ww2 = ww2;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(medewerker);
	}

}
