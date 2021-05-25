
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.palab;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.pingpong.PingPongInput;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanRegioOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = {
		Recht.GEBRUIKER_PA_LABORATORIA_BEHEER },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON,
		Bevolkingsonderzoek.CERVIX })
public class AanvullendePaLabGegevensPage extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private AutorisatieService autorisatieService;

	public AanvullendePaLabGegevensPage()
	{
		Instelling organisatie = getCurrentSelectedOrganisatie();
		Actie actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie, Recht.GEBRUIKER_PA_LABORATORIA_BEHEER);
		final boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(organisatie)));

		final IModel<PaLaboratorium> model = ModelUtil.cModel((PaLaboratorium) organisatie);
		setDefaultModel(model);

		Form<Void> form = new Form<>("form");
		add(form);

		SimpleListHibernateModel<ColoscopieLocatie> choices = new SimpleListHibernateModel<>(instellingService.getActieveInstellingen(ColoscopieLocatie.class));
		final PingPongInput<ColoscopieLocatie> coloscopielocaties = new PingPongInput<ColoscopieLocatie>("coloscopielocaties",
			new PropertyModel<List<ColoscopieLocatie>>(model, "coloscopielocaties"), choices, new ChoiceRenderer<ColoscopieLocatie>("naam", "id")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(ColoscopieLocatie object)
				{
					Object returnValue = super.getDisplayValue(object);
					if (object.getParent() != null)
					{
						returnValue = returnValue.toString() + " (" + object.getParent().getNaam() + ")";
					}
					return returnValue;
				}
			}, inzien)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<ColoscopieLocatie> model(ColoscopieLocatie object)
			{
				return ModelUtil.sModel(object);
			}
		};
		form.add(coloscopielocaties.setEnabled(!inzien || CollectionUtils.isEmpty(model.getObject().getColoscopielocaties())));
		form.add(new KoppelAanRegioOrganisatiePanel<PaLaboratorium>("regio", model).setEnabled(!inzien));
		form.add(new TextField<>("fqdn").add(new StringValidator(0, 255)).setEnabled(!inzien));

		form.add(new AjaxSubmitLink("submit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Instelling instelling = model.getObject();
				instellingService.saveOrUpdate(instelling);
				BasePage.markeerFormulierenOpgeslagen(target);
				this.info("Gegevens zijn succesvol opgeslagen");
			}

			@Override
			public boolean isVisible()
			{
				return !inzien;
			}

		});

		AjaxLink<Gebruiker> annuleren = new AjaxLink<Gebruiker>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}

			@Override
			public boolean isVisible()
			{
				return !inzien;
			}
		};
		form.add(annuleren);
	}
}
