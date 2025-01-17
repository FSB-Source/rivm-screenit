package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen;

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

import java.util.Arrays;

import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaNevenbevindingen;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public class NevenbevindingenBeoordelingPanel extends GenericPanel<MammaLezing> implements IDetachable
{
	private final IModel<MammaBeoordeling> beoordeling;

	private WebMarkupContainer nevenbevindingOpmerkingContainer;

	private WebMarkupContainer eersteLezingNevenBevindingenContainer;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	public NevenbevindingenBeoordelingPanel(String id, IModel<MammaLezing> model, IModel<MammaBeoordeling> beoordeling, boolean alleenLezen)
	{
		super(id, model);
		this.beoordeling = beoordeling;

		createNevenbevindingenMultiselect(alleenLezen);
		createEersteLezingResultaat(beoordeling.getObject());

		nevenbevindingOpmerkingContainer = new WebMarkupContainer("nevenbevindingOpmerkingContainer");
		nevenbevindingOpmerkingContainer.setOutputMarkupId(true);
		nevenbevindingOpmerkingContainer.setOutputMarkupPlaceholderTag(true);
		nevenbevindingOpmerkingContainer.setVisible(!getModelObject().getNevenbevindingen().isEmpty());
		add(nevenbevindingOpmerkingContainer);

		TextArea nevenbevindingOpmerking = new TextArea("nevenbevindingOpmerking");
		nevenbevindingOpmerking.add(StringValidator.maximumLength(HibernateMagicNumber.L255));
		nevenbevindingOpmerking.setOutputMarkupId(true);
		nevenbevindingOpmerkingContainer.add(nevenbevindingOpmerking);

		nevenbevindingOpmerking.setEnabled(!alleenLezen);
	}

	private void createEersteLezingResultaat(MammaBeoordeling beoordeling)
	{

		eersteLezingNevenBevindingenContainer = new WebMarkupContainer("eersteLezingNevenBevindingenContainer");
		eersteLezingNevenBevindingenContainer.setOutputMarkupPlaceholderTag(true);
		eersteLezingNevenBevindingenContainer.setOutputMarkupId(true);
		eersteLezingNevenBevindingenContainer.setVisible(false);
		add(eersteLezingNevenBevindingenContainer);

		if (isTweedeLezingEnHeeftEersteLezingNevenbevinding(beoordeling))
		{
			MammaLezing eersteLezing = beoordeling.getEersteLezing();
			String nevenbevindingen = baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, eersteLezing);
			String nevenbevindingenOpmerking = StringUtils.defaultIfBlank(eersteLezing.getNevenbevindingOpmerking(), "");
			eersteLezingNevenBevindingenContainer.add(new Label("eersteLezingNevenbevindingen", nevenbevindingen));
			eersteLezingNevenBevindingenContainer.add(new Label("eersteLezingNevenbevindingenOpmerking", nevenbevindingenOpmerking)
				.setVisible(StringUtils.isNotBlank(nevenbevindingenOpmerking)));
		}
	}

	private boolean isTweedeLezingEnHeeftEersteLezingNevenbevinding(MammaBeoordeling beoordeling)
	{
		MammaLezing eersteLezing = beoordeling.getEersteLezing();
		return (MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus())
			|| MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()))
			&& eersteLezing != null &&
			!eersteLezing.getNevenbevindingen().isEmpty();
	}

	private void createNevenbevindingenMultiselect(boolean alleenLezen)
	{
		ScreenitListMultipleChoice<MammaNevenbevindingen> nevenbevindingenSelector = new ScreenitListMultipleChoice<>("nevenbevindingen",
			alleenLezen ? Arrays.asList(MammaNevenbevindingen.values()) : MammaNevenbevindingen.getActieveNevenbevindingen(), new EnumChoiceRenderer<>());
		nevenbevindingenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				boolean huidigeLezingHeeftNevenBevindingen = !getModelObject().getNevenbevindingen().isEmpty();
				nevenbevindingOpmerkingContainer.setVisible(huidigeLezingHeeftNevenBevindingen);
				eersteLezingNevenBevindingenContainer.setVisible(isTweedeLezingEnHeeftEersteLezingNevenbevinding(beoordeling.getObject()) && huidigeLezingHeeftNevenBevindingen);
				target.add(nevenbevindingOpmerkingContainer, eersteLezingNevenBevindingenContainer);
			}
		});
		nevenbevindingenSelector.setEnabled(!alleenLezen);
		add(nevenbevindingenSelector);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(this.beoordeling);
	}
}
