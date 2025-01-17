package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.intakelocatie;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ScreenitAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanParentOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanRegioOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = {
	Recht.GEBRUIKER_BEHEER_CC_LOCATIES }, level = ToegangLevel.INSTELLING, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonIntakekamerBeheer extends OrganisatieBeheer
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ColonIntakelocatieService intakelocatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private OrganisatieParameterService organisatieParameterService;

	private final boolean inzien;

	public ColonIntakekamerBeheer()
	{
		var organisatie = getCurrentSelectedOrganisatie();
		var actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie, Recht.GEBRUIKER_BEHEER_CC_LOCATIES);
		inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.cRModel(organisatie)));
		add(new ColonIntakekamersEditPanel("kamers", ModelUtil.cModel((ColonIntakelocatie) organisatie))
		{
			@Override
			protected void onSaveOrUpdateKamers(AjaxRequestTarget target, ColonIntakelocatie intakelocatie)
			{
				BasePage.markeerFormulierenOpgeslagen(target);
				instellingService.saveOrUpdateColoscopieCentrum(intakelocatie);
				info("Kamergegevens zijn opgeslagen");
			}
		});

		addRoosterblokGrootteWijzigen();
		addLocatieBeschrijvingForm();
		addDigitaleIntakeForm();
		addGeprognostiseerdeRoosterBlokken();
	}

	private void addLocatieBeschrijvingForm()
	{
		var intakelocatie = (ColonIntakelocatie) getCurrentSelectedOrganisatie();

		var locatieBeschrijvingForm = new Form<>("locatieBeschrijvingForm", ModelUtil.ccModel(intakelocatie));
		add(locatieBeschrijvingForm);

		String locatieBeschrijving = organisatieParameterService.getOrganisatieParameter(intakelocatie, OrganisatieParameterKey.COLON_INTAKELOCATIE_BESCHRIJVING);
		var locatieBeschrijvingModel = Model.of(locatieBeschrijving);
		locatieBeschrijvingForm.add(new TextArea<>("locatieBeschrijving", locatieBeschrijvingModel).add(StringValidator.maximumLength(2048)));
		locatieBeschrijvingForm
			.add(new KoppelAanParentOrganisatiePanel<>("parent", locatieBeschrijvingForm.getModel()).setEnabled(!inzien || intakelocatie.getParent() == null));
		locatieBeschrijvingForm.add(new KoppelAanRegioOrganisatiePanel<>("regio", locatieBeschrijvingForm.getModel()).setEnabled(!inzien));
		locatieBeschrijvingForm.add(new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				intakelocatieService.saveIntakelocatieBeschrijving((ColonIntakelocatie) getForm().getDefaultModelObject(), locatieBeschrijvingModel.getObject(),
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				info("Locatiebeschrijving en/of koppeling met zorginstelling zijn opgeslagen");
				BasePage.markeerFormulierenOpgeslagen(target);
			}
		});
	}

	private void addDigitaleIntakeForm()
	{
		var intakelocatie = (ColonIntakelocatie) getCurrentSelectedOrganisatie();
		var digitaleIntakeForm = new Form<>("digitaleIntakeForm", ModelUtil.sModel(intakelocatie));
		add(digitaleIntakeForm);

		String digitaleIntakeTekst = organisatieParameterService.getOrganisatieParameter(intakelocatie, OrganisatieParameterKey.COLON_DIGITALE_INTAKE);
		var digitaleIntakeTekstModel = Model.of(digitaleIntakeTekst);
		digitaleIntakeForm.add(new TextArea<>("digitaleIntake", digitaleIntakeTekstModel).add(StringValidator.maximumLength(2048)));

		digitaleIntakeForm.add(new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				intakelocatieService.saveIntakelocatieDigitaleIntake((ColonIntakelocatie) getForm().getDefaultModelObject(), digitaleIntakeTekstModel.getObject(),
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				info("Digitale intake informatie is opgeslagen");
				BasePage.markeerFormulierenOpgeslagen(target);
			}
		});
	}

	private void addRoosterblokGrootteWijzigen()
	{
		Integer afspraakInMinuten = organisatieParameterService.getOrganisatieParameter(getCurrentSelectedOrganisatie(), OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN);
		ScreenitForm<Integer> form = new ScreenitForm<>("form");
		ArrayList<Integer> choices = new ArrayList<>();
		for (int i = 5; i <= 60; i = i + 5)
		{
			choices.add(i);
		}
		var model = Model.of(afspraakInMinuten);
		ScreenitDropdown<Integer> dropDown = new ScreenitDropdown<>("duurAfspraakInMinuten", model, choices);
		dropDown.setRequired(true);
		form.add(dropDown);
		ScreenitAjaxLink opslaan = new ScreenitAjaxLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				info("De tijdsduur afspraak is aangepast");
				var parameter = organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, model.getObject().toString(),
					getCurrentSelectedOrganisatie());
				organisatieParameterService.saveOrUpdateOrganisatieParameters(List.of(parameter), ScreenitSession.get().getLoggedInInstellingGebruiker());
			}
		};
		form.add(opslaan);

		if (inzien)
		{
			dropDown.setEnabled(false);
			opslaan.setVisible(false);
		}
		add(form);
	}

	private void addGeprognostiseerdeRoosterBlokken()
	{
		Form<ColonIntakelocatie> aantalGeprognRoosterBlokkenForm = new Form<>("aantalGeprognRoosterBlokkenForm",
			ModelUtil.cModel((ColonIntakelocatie) getCurrentSelectedOrganisatie()));
		add(aantalGeprognRoosterBlokkenForm);

		TextField<Integer> aantalGeprognostiseerdeRoosterblokken = new TextField<Integer>("aantalGeprognostiseerdeRoosterblokken");
		TextField<Integer> aantalGeprognostiseerdeRoosterblokkenVolgendJaar = new TextField<Integer>("aantalGeprognostiseerdeRoosterblokkenVolgendJaar");
		aantalGeprognRoosterBlokkenForm.add(aantalGeprognostiseerdeRoosterblokken);
		aantalGeprognRoosterBlokkenForm.add(aantalGeprognostiseerdeRoosterblokkenVolgendJaar);
		ScreenitAjaxLink opslaan = new ScreenitAjaxLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				info("De aantal geprognostiseerde roosterbrokken is aangepast");
				hibernateService.saveOrUpdate((HibernateObject) getForm().getDefaultModelObject());
			}
		};
		aantalGeprognRoosterBlokkenForm.add(opslaan);

		if (inzien)
		{
			aantalGeprognostiseerdeRoosterblokken.setEnabled(false);
			aantalGeprognostiseerdeRoosterblokkenVolgendJaar.setEnabled(false);
			opslaan.setVisible(false);
		}
		add(aantalGeprognRoosterBlokkenForm);
	}
}
