package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaCeZoekPanel extends GenericPanel<MammaCeWerklijstZoekObject>
{
	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	private final AbstractMammaCeWerklijst werklijst;

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenhedenSelector;

	private final WebMarkupContainer resultTable;

	private final Form<MammaCeWerklijstZoekObject> zoekForm;

	private ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingseenhedenSelector;

	private boolean toonSeFilter;

	public MammaCeZoekPanel(String id, IModel<MammaCeWerklijstZoekObject> model, AbstractMammaCeWerklijst werklijst, WebMarkupContainer resultTable)
	{
		super(id, model);
		this.werklijst = werklijst;
		this.resultTable = resultTable;
		zoekForm = new Form<>("form", getModel());
		createZoekForm();
		zetZoekobjectModel();
	}

	private void createZoekForm()
	{
		List<MammaCeFilter> teTonenCeFilters = getTeTonenCeFilters();
		toonSeFilter = teTonenCeFilters.contains(MammaCeFilter.SE);
		List<CentraleEenheid> mogelijkeCentraleEenheden = instellingService.getMogelijkeCentraleEenheden(ScreenitSession.get().getInstelling());
		List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden(mogelijkeCentraleEenheden);

		WebMarkupContainer centraleEenhedenContainer = new WebMarkupContainer("centraleEenhedenContainer");
		centraleEenhedenContainer.add(createCentraleEenhedenSelector(mogelijkeCentraleEenheden));
		centraleEenhedenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.CE));
		centraleEenhedenContainer.setOutputMarkupId(true);

		WebMarkupContainer beoordelingsEenhedenContainer = new WebMarkupContainer("beoordelingsEenhedenContainer");
		beoordelingseenhedenSelector = createBeoordelingseenhedenSelector(mogelijkeBeoordelingsEenheden);
		beoordelingsEenhedenContainer.add(beoordelingseenhedenSelector);
		beoordelingsEenhedenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.BE));
		beoordelingsEenhedenContainer.setOutputMarkupId(true);

		WebMarkupContainer screeningsEenhedenContainer = new WebMarkupContainer("screeningsEenhedenContainer");
		screeningsEenhedenSelector = createScreeningsEenhedenSelector(toonSeFilter ? mogelijkeBeoordelingsEenheden : new ArrayList<>());
		screeningsEenhedenContainer.add(screeningsEenhedenSelector);
		screeningsEenhedenContainer.setVisible(toonSeFilter);
		screeningsEenhedenContainer.setOutputMarkupId(true);

		WebMarkupContainer beoordelingStatussenContainer = new WebMarkupContainer("beoordelingStatussenContainer");
		ScreenitListMultipleChoice<MammaBeoordelingStatus> onderzoekStatusSelector = new ScreenitListMultipleChoice<>("beoordelingStatussen",
			getMammaMogelijkeBeoordelingFilterStatussen(), new EnumChoiceRenderer<>(this));
		beoordelingStatussenContainer.add(onderzoekStatusSelector);
		beoordelingStatussenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.STATUS));
		beoordelingStatussenContainer.setOutputMarkupId(true);

		zoekForm.add(centraleEenhedenContainer);
		zoekForm.add(beoordelingsEenhedenContainer);
		zoekForm.add(screeningsEenhedenContainer);
		zoekForm.add(beoordelingStatussenContainer);
		addZoekButton();
		add(zoekForm);
	}

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> createScreeningsEenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		List<MammaScreeningsEenheid> mogelijkeScreeningsEenheden = getMogelijkeScreeningsEenheden(mogelijkeBeoordelingsEenheden);
		return new ScreenitListMultipleChoice<>("screeningsEenheden", ModelUtil.listRModel(mogelijkeScreeningsEenheden), new ChoiceRenderer<>("naam"));
	}

	private void addZoekButton()
	{
		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				zetZoekobjectModel();
				target.add(resultTable, zoekForm);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	private List<MammaScreeningsEenheid> getMogelijkeScreeningsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		return toonSeFilter
			? beoordelingService.zoekScreeningsEenhedenMetCeWerklijstBeoordeling(getMammaMogelijkeBeoordelingFilterStatussen(), beoordelingsEenheden)
			: new ArrayList<>();
	}

	private void zetZoekobjectModel()
	{
		MammaCeWerklijstZoekObject zoekObject = getModelObject();
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingStatussen()))
		{
			zoekObject.setBeoordelingStatussen(getMammaMogelijkeBeoordelingFilterStatussen());
			zoekObject.getBeoordelingStatussen().removeAll(getRemoveFromDefaultFilter());
		}
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingsEenheden()))
		{
			zoekObject.setBeoordelingsEenheden(getMogelijkeBeoordelingsEenheden());
		}
		if (CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
		{
			resetSeKeuzelijst();
		}
		ScreenitSession.get().setZoekObject(werklijst.getPageClass(), zoekForm.getModel());
	}

	protected abstract List<MammaBeoordelingStatus> getRemoveFromDefaultFilter();

	protected abstract List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen();

	protected List<MammaCeFilter> getTeTonenCeFilters()
	{
		return Arrays.asList(MammaCeFilter.CE, MammaCeFilter.BE, MammaCeFilter.SE, MammaCeFilter.STATUS);
	}

	private ScreenitListMultipleChoice<CentraleEenheid> createCentraleEenhedenSelector(List<CentraleEenheid> mogelijkeCentraleEenheden)
	{
		ScreenitListMultipleChoice<CentraleEenheid> centraleEenhedenSelector = new ScreenitListMultipleChoice<>("centraleEenheden",
			ModelUtil.listRModel(mogelijkeCentraleEenheden), new ChoiceRenderer<>("naam"));
		centraleEenhedenSelector.setRequired(true);

		centraleEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetBeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return centraleEenhedenSelector;
	}

	private void resetBeKeuzelijst()
	{
		MammaCeWerklijstZoekObject zoekObject = getModelObject();
		List<CentraleEenheid> gekozenCEs = zoekObject.getCentraleEenheden();
		List<BeoordelingsEenheid> mogelijkeBEs = getMogelijkeBeoordelingsEenheden(gekozenCEs);
		beoordelingseenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeBEs));
		zoekObject.setBeoordelingsEenheden(mogelijkeBEs);
		resetSeKeuzelijst();
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden(List<CentraleEenheid> gekozenCEs)
	{
		return gekozenCEs == null || gekozenCEs.isEmpty() ? beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling())
			: beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling(), gekozenCEs);
	}

	private ScreenitListMultipleChoice<BeoordelingsEenheid> createBeoordelingseenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingsEenhedenSelector = new ScreenitListMultipleChoice<>("beoordelingsEenheden",
			ModelUtil.listRModel(mogelijkeBeoordelingsEenheden), new ChoiceRenderer<>("naam"));
		beoordelingsEenhedenSelector.setRequired(true);
		beoordelingsEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetSeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return beoordelingsEenhedenSelector;
	}

	private void resetSeKeuzelijst()
	{
		MammaCeWerklijstZoekObject zoekObject = getModelObject();
		List<BeoordelingsEenheid> gekozenBEs = zoekObject.getBeoordelingsEenheden();
		List<MammaScreeningsEenheid> mogelijkeSEs = getMogelijkeScreeningsEenheden(gekozenBEs);
		screeningsEenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeSEs));
		zoekObject.setScreeningsEenheden(mogelijkeSEs);
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden()
	{
		return beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling());
	}
}
