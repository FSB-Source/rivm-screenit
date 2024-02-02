package nl.rivm.screenit.main.web.gebruiker.login;

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

import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitWachtwoordField;
import nl.rivm.screenit.main.web.component.validator.ScreenITWachtwoordValidator;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.WachtwoordService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class PasswordChangePage extends LoginBasePage
{

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private WachtwoordService wachtwoordService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private GebruikerRepository gebruikerRepository;

	private final String gebruikersnaam;

	private final String changeCode;

	private String wachtwoord1;

	private String wachtwoord2;

	private IModel<Gebruiker> medewerker;

	public PasswordChangePage(PageParameters pageParameters)
	{
		ScreenitSession.get().replaceSession();
		changeCode = pageParameters.get("code").toString();
		gebruikersnaam = pageParameters.get("user").toString();

		var gebruiker = gebruikerRepository.findByWachtwoordChangeCode(changeCode).orElse(null);
		if (gebruiker != null)
		{
			var gebruikerByGebruikersnaam = gebruikerRepository.findByGebruikersnaam(gebruikersnaam);

			if (gebruikerByGebruikersnaam.isPresent() && gebruikerByGebruikersnaam.get().equals(gebruiker))
			{
				medewerker = ModelUtil.sModel(gebruiker);
				if (!isValidChangeCode())
				{
					error(getString("error.code.niet.meer.geldig"));
				}
			}
			else
			{
				error(getString("error.password.change.wrong.username"));
			}
		}
		else
		{
			error(getString("error.code.incorrect"));
		}

		final ScreenitForm<PasswordChangePage> form = new ScreenitForm<>("requestForm", new CompoundPropertyModel<>(this));
		final BookmarkablePageLink<Void> naarinlogpagina = new BookmarkablePageLink<>("naarinlogpagina", Application.get().getHomePage());
		AjaxButton opslaan = new AjaxButton("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Gebruiker gebruiker = ModelUtil.nullSafeGet(medewerker);
				if (!isValidChangeCode())
				{
					error("Code is niet meer geldig. Neem contact op met de beheerder.");
				}
				else if (!wachtwoord1.equals(wachtwoord2))
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
						wachtwoordService.setWachtwoord(gebruiker, wachtwoord1);

						gebruiker.setDatumWachtwoordAanvraag(null);
						gebruiker.setWachtwoordChangeCode(null);

						hibernateService.saveOrUpdate(gebruiker);
						logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GEWIJZIGD, gebruiker);
						info("Wachtwoord is opgeslagen");
						naarinlogpagina.setVisible(Boolean.TRUE);
						setVisible(Boolean.FALSE);

						for (Component component : form)
						{
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
		FormComponent<String> gebruikersnaamTf = ComponentHelper.addTextField(form, "gebruikersnaam", true, 50, true);
		gebruikersnaamTf.add(new FocusBehavior());

		ComponentHelper.addTextField(form, "changeCode", true, 50, true);

		ScreenITWachtwoordValidator validator = new ScreenITWachtwoordValidator(gebruikersnaamTf, true, medewerker);
		ScreenitWachtwoordField wachtwoord1Field = new ScreenitWachtwoordField("wachtwoord1", new PropertyModel<>(this, "wachtwoord1"), true, validator);
		form.add(wachtwoord1Field);

		ScreenitWachtwoordField wachtwoord2Field = new ScreenitWachtwoordField("wachtwoord2", new PropertyModel<>(this, "wachtwoord2"), true, null);
		form.add(wachtwoord2Field);
	}

	private boolean isValidChangeCode()
	{
		var nu = currentDateSupplier.getLocalDateTime();
		var gebruiker = ModelUtil.nullSafeGet(medewerker);
		if (gebruiker != null)
		{
			var aanvraagDatum = gebruiker.getDatumWachtwoordAanvraag();
			return Duration.between(DateUtil.toLocalDateTime(aanvraagDatum), nu).toDays() < 1;
		}
		else
		{
			return false;
		}

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(medewerker);
	}

}
