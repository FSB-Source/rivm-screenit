package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats;

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

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPostcodeReeksEditPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPostcodeReeksenPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaStandplaatsOpmerkingenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaStandplaatsEditPage extends MammaPlanningBasePage
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaStandplaatsEditPage.class);

	@SpringBean
	private MammaStandplaatsService standplaatsService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private InstellingService instellingService;

	private WebMarkupContainer persistentContainer;

	private BootstrapDialog dialog;

	private boolean ingelogdNamensRegio;

	private boolean magAanpassen;

	private WebMarkupContainer mainContainer;

	private IModel<ScreeningOrganisatie> initieleScreeningOrganisatieModel;

	public MammaStandplaatsEditPage(IModel<MammaStandplaats> model)
	{
		setDefaultModel(model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);
		ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie() != null;
		initieleScreeningOrganisatieModel = ModelUtil.sModel(model.getObject().getRegio());

		addOrReplaceMainForm(null);

		persistentContainer = new WebMarkupContainer("persistent");
		persistentContainer.setOutputMarkupId(true);
		persistentContainer.setOutputMarkupPlaceholderTag(true);
		add(persistentContainer);
		if (model.getObject().getId() == null)
		{
			persistentContainer.setVisible(false);
		}
		else
		{
			persistentContainer.add(new MammaStandplaatsOpmerkingenPanel("opmerkingen", model));
			addLocaties();
			addPostcodeReeksen();
		}
		add(new IndicatingAjaxLink<Void>("terug")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaStandplaatsZoekenPage.class);
			}
		});
	}

	private void addOrReplaceMainForm(AjaxRequestTarget target)
	{
		WebMarkupContainer nieuwMainContainer = new WebMarkupContainer("mainContainer");
		nieuwMainContainer.setOutputMarkupId(true);
		ScreenitForm<MammaStandplaats> mainForm = new ScreenitForm<>("mainForm", (IModel<MammaStandplaats>) getDefaultModel());
		mainForm.setOutputMarkupId(true);
		nieuwMainContainer.add(mainForm);
		ComponentHelper.addTextField(mainForm, "naam", true, 255, String.class, !magAanpassen || !ingelogdNamensRegio)
			.add(new UniqueFieldValidator<>(MammaStandplaats.class, getStandplaats().getId(), "naam", hibernateService));

		mainForm.add(new Label("gekoppeldeSEs", screeningsEenheidService.getGekoppeldeScreeningsEenhedenTekst(getStandplaats())));

		mainForm.add(new ScreenitDropdown<>("regio", ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), false),
			new ChoiceRenderer<>("naam")).setVisible(!ingelogdNamensRegio).setEnabled(magAanpassen && !ingelogdNamensRegio));

		addOpslaanStandplaats(mainForm);
		addInActiverenButton(mainForm);
		if (target == null)
		{
			mainContainer = nieuwMainContainer;
			add(mainContainer);
		}
		else
		{
			mainContainer.replaceWith(nieuwMainContainer);
			mainContainer = nieuwMainContainer;
			target.add(mainContainer);
		}

	}

	private void addOpslaanStandplaats(ScreenitForm<MammaStandplaats> mainForm)
	{
		mainForm.add(new IndicatingAjaxButton("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaStandplaats standplaats = mainForm.getModelObject();

				if (!standplaats.getRegio().equals(initieleScreeningOrganisatieModel.getObject()) && standplaatsService.countActieveStandplaatsPeriodes(standplaats) > 0)
				{
					error(getString("heeft.actieve.standplaatsperiodes"));
					return;
				}

				boolean changed = standplaatsService.saveOrUpdateStandplaats(standplaats, ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (changed)
				{
					if (!persistentContainer.isVisible())
					{
						persistentContainer.setVisible(true);
						target.add(persistentContainer);
						addOrReplaceMainForm(target);
						persistentContainer.addOrReplace(new MammaStandplaatsOpmerkingenPanel("opmerkingen", mainForm.getModel()));
						addLocaties();
						addPostcodeReeksen();
					}
					success(getString("message.gegevensopgeslagen"));
					BasePage.markeerFormulierenOpgeslagen(target);
				}
			}
		}.setVisible(magAanpassen));
	}

	private void addInActiverenButton(ScreenitForm<MammaStandplaats> mainForm)
	{
		AjaxLink<Gebruiker> inActiveren = new ConfirmingIndicatingAjaxLink<Gebruiker>("inActiveren", dialog, "question.remove.standplaats")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaStandplaats standplaats = getStandplaats();

				boolean activeren = Boolean.FALSE.equals(standplaats.getActief());
				if (activeren)
				{
					if (standplaats.getLocatie().getPostcodeCoordinaten() == null || standplaats.getLocatie().getHuisnummer() == null)
					{
						error(getString("huisnummer.coordinaten.niet.ingevuld"));
						return;
					}
				}

				standplaats.setActief(activeren);
				standplaatsService.saveOrUpdateStandplaats(standplaats, ScreenitSession.get().getLoggedInInstellingGebruiker());

				setResponsePage(MammaStandplaatsZoekenPage.class);
				if (standplaats.getActief())
				{
					ScreenitSession.get().info(getString("is.geactiveerd"));
				}
			}

			@Override
			protected boolean skipConfirmation()
			{
				return Boolean.FALSE.equals(getStandplaats().getActief());
			}

		};

		MammaStandplaats standplaats = getStandplaats();

		if (Boolean.FALSE.equals(standplaats.getActief()))
		{
			inActiveren.add(new Label("inActiverenTitle", "Activeren"));
		}
		else
		{
			inActiveren.add(new Label("inActiverenTitle", "Inactiveren"));
			String inactiverenProperty = standplaatsService.magStandplaatsInactiveren(standplaats);
			boolean heeftTehuizen = standplaats.getTehuizen().size() > 0;

			if (heeftTehuizen)
			{
				inactiverenProperty = "inactiveren.title.heefttehuizen";
			}
			if (StringUtils.isNotBlank(inactiverenProperty))
			{
				inActiveren.setEnabled(false);
				inActiveren.add(new AttributeAppender("title", Model.of(getString(inactiverenProperty))));
			}
		}
		boolean magInActiveren = standplaats.getId() != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.VERWIJDEREN)
			&& ingelogdNamensRegio;
		inActiveren.setVisible(magInActiveren);

		mainForm.add(inActiveren);
	}

	private void addLocaties()
	{
		addLocatie("locatie");
		addLocatie("tijdelijkeLocatie");
	}

	private void addLocatie(String property)
	{
		persistentContainer.add(new MammaStandplaatsViewLocatieAdresPanel(property, new CompoundPropertyModel<>(new PropertyModel<>(getDefaultModel(), property)),
			(IModel<MammaStandplaats>) getDefaultModel(), dialog));
	}

	private void addPostcodeReeksen()
	{
		AjaxLink<Void> toevoegen = new IndicatingAjaxLink<Void>("postcodeReeksToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaPostcodeReeks postcodeReeks = new MammaPostcodeReeks();
				IModel<MammaPostcodeReeks> model = ModelUtil.cModel(postcodeReeks);
				model.getObject().setStandplaats(getStandplaats());
				setResponsePage(new MammaPostcodeReeksEditPage(model)
				{

					@Override
					protected void terug(AjaxRequestTarget target, IModel<MammaPostcodeReeks> model)
					{
						setResponsePage(new MammaStandplaatsEditPage(
							ModelUtil.cModel((MammaStandplaats) HibernateHelper.deproxy(ModelProxyHelper.deproxy(model.getObject().getStandplaats())))));
					}
				});
			}
		};
		toevoegen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.TOEVOEGEN) && ingelogdNamensRegio);
		persistentContainer.add(toevoegen);

		MammaPostcodeReeks zoekObject = new MammaPostcodeReeks();
		IModel<MammaPostcodeReeks> criteriaModel = ModelUtil.cModel(zoekObject);
		zoekObject = criteriaModel.getObject();

		zoekObject.setStandplaats(getStandplaats());

		persistentContainer.add(new MammaPostcodeReeksenPanel("reeksen", criteriaModel, false));
	}

	private MammaStandplaats getStandplaats()
	{
		return (MammaStandplaats) getDefaultModelObject();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(initieleScreeningOrganisatieModel);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaStandplaatsZoekenPage.class;
	}
}
