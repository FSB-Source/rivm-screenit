package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.blokkade;

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

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;
import nl.rivm.screenit.service.mamma.MammaBaseBlokkadeService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class MammaBlokkadeEditPanel extends GenericPanel<MammaBlokkade>
{

	@SpringBean
	private MammaScreeningsEenheidDao screeningsEenheidDao;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	@SpringBean
	private MammaBaseBlokkadeService baseBlokkadeService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	private IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private Form form;

	public MammaBlokkadeEditPanel(String id, @Nullable final IModel<MammaBlokkade> blokkadeModel,
		@Nullable IModel<MammaScreeningsEenheid> screeningsEenheidModel)
	{
		super(id, blokkadeModel);
		this.screeningsEenheidModel = screeningsEenheidModel;

		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		boolean magAanpassen = ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);

		if (blokkadeModel == null)
		{
			MammaBlokkade blokkade = new MammaBlokkade();
			setModel(ModelUtil.cModel(blokkade));
			blokkade = getModelObject();
			blokkade.setActief(true);
			blokkade.setType(MammaBlokkadeType.SCREENINGS_EENHEID);
			if (screeningsEenheidModel != null)
			{
				blokkade.setScreeningsEenheid(screeningsEenheidModel.getObject());
			}
		}

		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new AjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});

		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();
		form = new Form("form");
		form.setEnabled(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN) && sessionSO != null);
		add(form);

		ScreenitDropdown<MammaBlokkadeType> type = new ScreenitDropdown<>("type", Arrays.asList(MammaBlokkadeType.values()),
			new EnumChoiceRenderer<>());
		form.add(type);

		addOrReplaceBlokkadeTypeSpecifics(null, getModelObject().getType());

		DatePicker<Date> vanaf = ComponentHelper.newDatePicker("vanaf");
		vanaf.setDisabled(!magAanpassen);
		form.add(vanaf.setRequired(true));

		DatePicker<Date> totEnMet = ComponentHelper.newDatePicker("totEnMet");
		totEnMet.setDisabled(!magAanpassen);
		form.add(totEnMet.setRequired(true));

		form.add(new DependantDateValidator(vanaf, totEnMet, DependantDateValidator.Operator.AFTER));

		ComponentHelper.addTextArea(form, "reden", false, HibernateMagicNumber.L4096, false);

		type.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				MammaBlokkade blokkade = MammaBlokkadeEditPanel.this.getModelObject();
				blokkade.setRegio(null);
				blokkade.setScreeningsEenheid(null);
				blokkade.setStandplaats(null);

				switch (blokkade.getType())
				{
				case SCREENINGS_ORGANISATIE:
					blokkade.setRegio(ScreenitSession.get().getScreeningOrganisatie());
					break;
				case SCREENINGS_EENHEID:
					if (MammaBlokkadeEditPanel.this.screeningsEenheidModel != null)
					{
						blokkade.setScreeningsEenheid(MammaBlokkadeEditPanel.this.screeningsEenheidModel.getObject());
					}
					break;

				}

				MammaBlokkadeEditPanel.this.addOrReplaceBlokkadeTypeSpecifics(target, blokkade.getType());
			}
		});

		final ConfirmingIndicatingAjaxSubmitLink inActiveren = new ConfirmingIndicatingAjaxSubmitLink("inActiveren", dialog,
			"popupOverlapMeldingInactiveren")
		{

			private static final long serialVersionUID = 1L;

			private StringBuilder errorMessage;

			@Override
			protected boolean skipConfirmation()
			{
				StringBuilder errorMessage = maakErrorMessage(MammaBlokkadeEditPanel.this.getModelObject());
				if (errorMessage.length() > 0)
				{
					this.errorMessage = errorMessage;
					return false;
				}
				return true;
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				return Model.of(errorMessage.toString());
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaBlokkade blokkade = MammaBlokkadeEditPanel.this.getModelObject();
				blokkade.setActief(!blokkade.getActief());
				baseBlokkadeService.saveOrUpdate(blokkade, ScreenitSession.get().getLoggedInInstellingGebruiker());
				blokkadeGewijzigd(target);
			}
		};

		form.add(inActiveren);
		inActiveren.setVisible(blokkadeModel != null);
		inActiveren.add(new Label("inActiverenLabel", getModelObject().getActief() ? "Inactiveren" : "Activeren"));

		form.add(new ConfirmingIndicatingAjaxSubmitLink("opslaan", dialog, "popupOverlapMelding")
		{

			private static final long serialVersionUID = 1L;

			private StringBuilder errorMessage;

			@Override
			protected boolean skipConfirmation()
			{
				StringBuilder errorMessage = maakErrorMessage(MammaBlokkadeEditPanel.this.getModelObject());
				if (errorMessage.length() > 0)
				{
					this.errorMessage = errorMessage;
					return false;
				}
				return true;
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				return Model.of(errorMessage.toString());
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				baseBlokkadeService.saveOrUpdate(MammaBlokkadeEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				blokkadeGewijzigd(target);
			}
		});
	}

	private void addOrReplaceBlokkadeTypeSpecifics(AjaxRequestTarget target, MammaBlokkadeType blokkadeType)
	{
		WebMarkupContainer regioContainer = new WebMarkupContainer("regioContainer");
		WebMarkupContainer screeningsEenheidContainer = new WebMarkupContainer("screeningsEenheidContainer");
		WebMarkupContainer standplaatsContainer = new WebMarkupContainer("standplaatsContainer");

		regioContainer.setOutputMarkupId(true);
		screeningsEenheidContainer.setOutputMarkupId(true);
		standplaatsContainer.setOutputMarkupId(true);

		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();

		switch (blokkadeType)
		{
		case SCREENINGS_ORGANISATIE:
			regioContainer.add(new Label("regio.naam"));

			screeningsEenheidContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			standplaatsContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			break;
		case SCREENINGS_EENHEID:
			if (screeningsEenheidModel == null)
			{
				ScreenitDropdown<MammaScreeningsEenheid> screeningsEenheid = new ScreenitDropdown<>("screeningsEenheid",
					ModelUtil.listRModel(screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO)), new ChoiceRenderer<>("naam"));
				screeningsEenheidContainer.add(screeningsEenheid.setRequired(true));
				screeningsEenheidContainer.add(new EmptyPanel("screeningsEenheid.naam"));
			}
			else
			{
				screeningsEenheidContainer.add(new EmptyPanel("screeningsEenheid").setVisible(false));
				screeningsEenheidContainer.add(new Label("screeningsEenheid.naam"));
			}

			regioContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			standplaatsContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			break;
		case STANDPLAATS:
			IModel<List<MammaStandplaats>> standplaatsenModel = ModelUtil.listRModel(standplaatsService.getActieveStandplaatsen(sessionSO));
			standplaatsContainer.add(new ScreenitDropdown<>("standplaats", standplaatsenModel, new ChoiceRenderer<>("naam")).setRequired(true));

			regioContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			screeningsEenheidContainer.setVisible(false).setOutputMarkupPlaceholderTag(true);
			break;
		}

		form.addOrReplace(regioContainer, screeningsEenheidContainer, standplaatsContainer);
		if (target != null)
		{
			target.add(regioContainer, screeningsEenheidContainer, standplaatsContainer);
		}
	}

	private StringBuilder maakErrorMessage(MammaBlokkade blokkade)
	{
		final SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		StringBuilder errorMessage = new StringBuilder();

		Map<MammaScreeningsEenheid, List<Date>> screeningsEenheidDateSetMap = afspraakService.getAfspraakDatums(blokkade);
		if (!screeningsEenheidDateSetMap.isEmpty())
		{
			errorMessage.append(getString("bestaande.afspraken"));
			for (Map.Entry<MammaScreeningsEenheid, List<Date>> entry : screeningsEenheidDateSetMap.entrySet())
			{
				errorMessage.append("<br />" + entry.getKey().getNaam() + ": ");
				errorMessage.append(String.join(", ", entry.getValue().stream().map(format::format).collect(Collectors.toList())));
			}
		}

		List<MammaBlokkade> overlappendeBlokkades = baseBlokkadeService.getOverlappendeBlokkades(blokkade);
		if (CollectionUtils.isNotEmpty(overlappendeBlokkades))
		{
			if (errorMessage.length() > 0)
			{
				errorMessage.append("<br />");
			}
			errorMessage.append(getString("overlapt.met.blokkade"));
			overlappendeBlokkades.forEach(overlappendeBlokkade -> errorMessage
				.append("<br />" + format.format(overlappendeBlokkade.getVanaf()) + " / " + format.format(overlappendeBlokkade.getTotEnMet())));
		}

		return errorMessage;
	}

	protected abstract void blokkadeGewijzigd(AjaxRequestTarget target);

	protected abstract void close(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel);
	}
}
