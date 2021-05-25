package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopiecentrum;

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

import java.util.ArrayList;

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
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = {
	Recht.GEBRUIKER_BEHEER_CC_LOCATIES }, level = ToegangLevel.INSTELLING, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColoscopieCentrumKamerBeheer extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private final boolean inzien;

	private final Actie actie;

	public ColoscopieCentrumKamerBeheer()
	{
		Instelling organisatie = getCurrentSelectedOrganisatie();
		actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie, Recht.GEBRUIKER_BEHEER_CC_LOCATIES);
		inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.cRModel(organisatie)));
		add(new KamersEditPanel("kamers", ModelUtil.cModel((ColoscopieCentrum) organisatie))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSaveOrUpdateKamers(AjaxRequestTarget target, ColoscopieCentrum coloscopieCentrum)
			{
				BasePage.markeerFormulierenOpgeslagen(target);
				instellingService.saveOrUpdateColoscopieCentrum(coloscopieCentrum);
				info("Kamersgegevens zijn opgeslagen");
			}
		});

		addRoosterblokGrootteWijzigen();
		addLocatieBeschrijvingForm();
		addGeprognostiseerdeRoosterBlokken();
	}

	private void addLocatieBeschrijvingForm()
	{
		Form<ColoscopieCentrum> locatieBeschrijvingForm = new Form<>("locatieBeschrijvingForm",
			new CompoundPropertyModel<>(ModelUtil.sModel((ColoscopieCentrum) getCurrentSelectedOrganisatie())));
		add(locatieBeschrijvingForm);

		locatieBeschrijvingForm.add(new TextArea<String>("locatieBeschrijving").add(StringValidator.maximumLength(2048)));
		Instelling organisatie = getCurrentSelectedOrganisatie();
		locatieBeschrijvingForm
			.add(new KoppelAanParentOrganisatiePanel<ColoscopieCentrum>("parent", locatieBeschrijvingForm.getModel()).setEnabled(!inzien || organisatie.getParent() == null));
		locatieBeschrijvingForm.add(new KoppelAanRegioOrganisatiePanel<ColoscopieCentrum>("regio", locatieBeschrijvingForm.getModel()).setEnabled(!inzien));
		locatieBeschrijvingForm.add(new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				info("Locatiebeschrijving en/of koppeling met zorginstelling zijn opgeslagen");
				BasePage.markeerFormulierenOpgeslagen(target);
				hibernateService.saveOrUpdate((HibernateObject) getForm().getDefaultModelObject());
			}
		});
	}

	private void addRoosterblokGrootteWijzigen()
	{
		ScreenitForm<AfspraakDefinitie> form = new ScreenitForm<>("form",
			new CompoundPropertyModel<AfspraakDefinitie>(new PropertyModel<AfspraakDefinitie>(ModelUtil.sModel(getCurrentSelectedOrganisatie()), "afspraakDefinities[0]")));
		ArrayList<Integer> choices = new ArrayList<Integer>();
		for (int i = 5; i <= 60; i = i + 5)
		{
			choices.add(Integer.valueOf(i));
		}
		ScreenitDropdown<Integer> dropDown = new ScreenitDropdown<Integer>("duurAfspraakInMinuten", choices);
		form.add(dropDown);
		ScreenitAjaxLink opslaan = new ScreenitAjaxLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				info("De tijdsduur afspraak is aangepast");
				hibernateService.saveOrUpdate((HibernateObject) form.getDefaultModelObject());
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
		Form<ColoscopieCentrum> aantalGeprognRoosterBlokkenForm = new Form<>("aantalGeprognRoosterBlokkenForm",
			ModelUtil.cModel((ColoscopieCentrum) getCurrentSelectedOrganisatie()));
		add(aantalGeprognRoosterBlokkenForm);

		TextField<Integer> aantalGeprognostiseerdeRoosterblokken = new TextField<Integer>("aantalGeprognostiseerdeRoosterblokken");
		TextField<Integer> aantalGeprognostiseerdeRoosterblokkenVolgendJaar = new TextField<Integer>("aantalGeprognostiseerdeRoosterblokkenVolgendJaar");
		aantalGeprognRoosterBlokkenForm.add(aantalGeprognostiseerdeRoosterblokken);
		aantalGeprognRoosterBlokkenForm.add(aantalGeprognostiseerdeRoosterblokkenVolgendJaar);
		ScreenitAjaxLink opslaan = new ScreenitAjaxLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

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
