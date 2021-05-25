package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaSignaleringAfbeeldingPanel;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaSignaleren;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaMBBBeoordelingPanel extends AbstractBEAccordionPanel<MammaOnderzoek>
{

	private WebMarkupContainer mbbSignaleringsafbeeldingContainer;

	private WebMarkupContainer afwijkingContainer;

	private final static String HEEFT_AFWIJKINGEN_SRC = "/assets/images/mamma/afwijkingen.png";

	private final static String HEEFT_GEEN_AFWIJKINGEN_SRC = "/assets/images/mamma/geen_afwijkingen.png";

	@SpringBean
	private MammaBaseAfbeeldingService afbeeldingService;

	private final boolean initieelToonMbbSignaleren;

	public MammaMBBBeoordelingPanel(String id, IModel<MammaOnderzoek> item, boolean toonMbbSignaleren)
	{
		super(id, item, Model.of("Signalering"), 4);
		this.initieelToonMbbSignaleren = toonMbbSignaleren;
		super.setIngeklapt(false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		createMbbPanel();
	}

	private void createMbbPanel()
	{
		setOutputMarkupId(true);

		boolean isAutoniem = !MammobridgeRole.anoniemeRollen().contains(ScreenitSession.get().getMammaHuidigeIDS7Role());

		panelContainer.add(new Label("signaleren.afgerondDoor.medewerker.naamVolledig").setVisible(isAutoniem));

		mbbSignaleringsafbeeldingContainer = new MammaSignaleringAfbeeldingPanel("mbbSignaleringAfbeelding", new PropertyModel<>(getModel(), "signaleren"));
		mbbSignaleringsafbeeldingContainer.setVisible(initieelToonMbbSignaleren);
		mbbSignaleringsafbeeldingContainer.setOutputMarkupPlaceholderTag(true);
		mbbSignaleringsafbeeldingContainer.setOutputMarkupId(true);
		panelContainer.add(mbbSignaleringsafbeeldingContainer);

		afwijkingContainer = new WebMarkupContainer("afwijking");
		afwijkingContainer.setVisible(initieelToonMbbSignaleren);
		afwijkingContainer.setOutputMarkupId(true);
		afwijkingContainer.setOutputMarkupPlaceholderTag(true);
		panelContainer.add(afwijkingContainer);

		MammaSignaleren signaleren = getModelObject().getSignaleren();
		boolean heeftAfwijkingen = signaleren != null && signaleren.getHeeftAfwijkingen();

		Label afwijkingLabel = new Label("afwijkingTekst", new Model<>(heeftAfwijkingen ? "Wel signalering" : "Geen signalering"));
		afwijkingContainer.add(afwijkingLabel);

		WebMarkupContainer afwijkingImage = new WebMarkupContainer("afwijkingImage");
		afwijkingImage.add(new AttributeModifier("src", new Model<>(heeftAfwijkingen ? HEEFT_AFWIJKINGEN_SRC : HEEFT_GEEN_AFWIJKINGEN_SRC)));

		afwijkingContainer.add(afwijkingImage);
	}

	public void reloadContent(AjaxRequestTarget target, boolean toonMbbSignaleren)
	{
		mbbSignaleringsafbeeldingContainer.setVisible(toonMbbSignaleren);
		afwijkingContainer.setVisible(toonMbbSignaleren);
		super.setIngeklapt(!toonMbbSignaleren);
		super.refreshPanel(target);
	}
}
