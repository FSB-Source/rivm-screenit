package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.MammaFollowUpConclusieChoice;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientPaspoortHorizontaal;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie.MammaFollowUpPathologieVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie.MammaFollowUpRadiologieVerslagInzienPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Objects;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_CONCLUSIE_WERKLIJST })
public class MammaFollowUpConclusiePage extends AbstractMammaFollowUpPage
{
	@SpringBean
	private MammaFollowUpService followUpService;

	private IModel<MammaFollowUpConclusieChoice> conclusieEnumModel;

	private final IModel<MammaScreeningRonde> screeningRondeModel;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/font-awesome/css/font-awesome.min.css"));
	}

	public MammaFollowUpConclusiePage(MammaScreeningRonde screeningRonde)
	{
		screeningRondeModel = new CompoundPropertyModel<>(ModelUtil.sModel(screeningRonde));
		setDefaultModel(screeningRondeModel);

		add(new ClientPaspoortHorizontaal("paspoort", new CompoundPropertyModel<>(ModelUtil.sModel(screeningRonde.getDossier().getClient()))));

		voegOnderzoekInformatieToe(screeningRonde);
		voegRadiologieVerslagenToe();
		voegFollowUpVerslagenToe();
		voegConclusieFormToe(screeningRonde);
	}

	private void voegConclusieFormToe(MammaScreeningRonde screeningRonde)
	{
		ScreenitForm<MammaScreeningRonde> conclusieForm = new ScreenitForm<>("conclusieForm");
		add(conclusieForm);

		conclusieEnumModel = new Model<>(initieleConclusieKeuze(screeningRonde));

		WebMarkupContainer statusContainer = new WebMarkupContainer("conclusieContainer");
		statusContainer.setOutputMarkupId(true);
		conclusieForm.add(statusContainer);

		statusContainer.add(new AjaxButtonGroup<>("conclusie", conclusieEnumModel,
			new ListModel<>(Arrays.asList(MammaFollowUpConclusieChoice.values())), new EnumChoiceRenderer<>())
		{
			@Override
			protected void onSelectionChanged(MammaFollowUpConclusieChoice selection, AjaxRequestTarget target, String markupId)
			{
				super.onSelectionChanged(selection, target, markupId);

				target.appendJavaScript("$('#" + markupId + "').parent().find('.btn').removeClass('red');");

				if (MammaFollowUpConclusieChoice.POSITIEF.equals(selection))
				{
					target.appendJavaScript("$('#" + markupId + "').addClass('red');");
				}
			}

			@Override
			protected void addExtraAttributes(IndicatingAjaxLink button, MammaFollowUpConclusieChoice selection)
			{
				super.addExtraAttributes(button, selection);

				if (MammaFollowUpConclusieChoice.POSITIEF.equals(selection))
				{
					button.add(new AttributeAppender("class", " red ")
					{
						@Override
						public boolean isEnabled(Component component)
						{
							return Objects.equal(getDefaultModelObject(), selection);
						}
					});
				}
			}

			@Override
			protected AjaxLink<String> createButton(String id, IModel<MammaFollowUpConclusieChoice> model)
			{
				AjaxLink<String> button = super.createButton(id, model);
				if (MammaFollowUpConclusieChoice.POSITIEF.equals(model.getObject()) && teTonenPaVerslagen().isEmpty())
				{
					button.setEnabled(false);
				}
				return button;
			}
		});

		conclusieForm.add(new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				if (conclusieEnumModel.getObject() == null)
				{
					error(getString("geen.keuze"));
				}
				else
				{
					MammaScreeningRonde screeningRonde = screeningRondeModel.getObject();
					MammaFollowUpConclusieStatus conclusieStatus = followUpService.bepaalFollowUpConclusie(screeningRonde, conclusieEnumModel.getObject());
					if (conclusieStatus != null)
					{
						followUpService.saveFollowUpConclusieStatus(screeningRonde, conclusieStatus, ScreenitSession.get().getLoggedInAccount());
					}
					setResponsePage(MammaFollowUpConclusieWerklijst.class);
				}
			}
		});
	}

	private MammaFollowUpConclusieChoice initieleConclusieKeuze(MammaScreeningRonde screeningRonde)
	{
		var conclusieStatus = screeningRonde.getFollowUpConclusieStatus();
		if (conclusieStatus == null)
		{
			return null;
		}

		switch (conclusieStatus)
		{
		case TRUE_POSITIVE:
		case FALSE_NEGATIVE:
			return MammaFollowUpConclusieChoice.POSITIEF;
		case TRUE_NEGATIVE:
		case FALSE_POSITIVE:
			return MammaFollowUpConclusieChoice.NEGATIEF;
		case NIET_TE_VERWACHTEN:
			return MammaFollowUpConclusieChoice.NIET_TE_VERWACHTEN;
		default:
			return null;
		}
	}

	private void voegFollowUpVerslagenToe()
	{
		ListView<MammaFollowUpVerslag> followUpVerslagList = new ListView<>("followUpVerslagen", ModelUtil.listRModel(teTonenPaVerslagen()))
		{
			@Override
			protected void populateItem(ListItem<MammaFollowUpVerslag> followUpVerslagListItem)
			{
				followUpVerslagListItem.add(new MammaFollowUpPathologieVerslagInzienPanel("paVerslag", followUpVerslagListItem.getModel()));
			}
		};
		add(followUpVerslagList);

	}

	private List<MammaFollowUpVerslag> teTonenPaVerslagen()
	{
		return followUpService.getAfgerondeFollowUpPathologieVerslagen(screeningRondeModel.getObject());
	}

	private void voegRadiologieVerslagenToe()
	{
		ListView<MammaFollowUpRadiologieVerslag> radiologieVerslagList = new ListView<>("followUpRadiologieVerslagen")
		{
			@Override
			protected void populateItem(ListItem<MammaFollowUpRadiologieVerslag> listItem)
			{
				listItem.add(new MammaFollowUpRadiologieVerslagInzienPanel("radiologieVerslag", listItem.getModel()));

			}
		};

		add(radiologieVerslagList);
	}

	private void voegOnderzoekInformatieToe(MammaScreeningRonde screeningRonde)
	{
		add(DateLabel.forDatePattern("laatsteUitnodiging.laatsteAfspraak.onderzoek.creatieDatum", Constants.DEFAULT_DATE_FORMAT));
		add(new EnumLabel<>("laatsteUitnodiging.laatsteAfspraak.onderzoek.laatsteBeoordeling.status"));

		WebMarkupContainer biradsContainer = new WebMarkupContainer("biradsContainer");
		MammaOnderzoek onderzoek = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
		MammaBeoordeling beoordeling = onderzoek.getLaatsteBeoordeling();
		MammaLezing verslagLezing = beoordeling != null ? beoordeling.getVerslagLezing() : null;
		MammaBIRADSWaarde biradsLinks = verslagLezing != null ? verslagLezing.getBiradsLinks() : null;
		MammaBIRADSWaarde biradsRechts = verslagLezing != null ? verslagLezing.getBiradsRechts() : null;
		biradsContainer.add(new Label("biradsLinks",
			biradsLinks != null ? MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.LINKER_BORST, biradsLinks) : ""));
		biradsContainer.add(new Label("biradsRechts",
			biradsRechts != null ? MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.RECHTER_BORST, biradsRechts) : ""));

		biradsContainer.setVisible(verslagLezing != null);
		add(biradsContainer);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(conclusieEnumModel);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaFollowUpConclusieWerklijst.class;
	}
}
