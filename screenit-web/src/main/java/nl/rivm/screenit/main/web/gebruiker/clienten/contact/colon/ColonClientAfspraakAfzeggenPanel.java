
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.RedenAfspraakAfzeggen;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ColonClientAfspraakAfzeggenPanel extends GenericPanel<ColonIntakeAfspraak>
{

	private static final long serialVersionUID = 1L;

	private IModel<Boolean> briefTegenhouden = Model.of(Boolean.FALSE);

	public ColonClientAfspraakAfzeggenPanel(String id, IModel<ColonIntakeAfspraak> afspraakModel)
	{
		super(id, afspraakModel);
		add(new Label("title"));
		add(DateLabel.forDatePattern("startTime", "dd-MM-yyyy HH:mm"));
		WebMarkupContainer clientPortaal = new WebMarkupContainer("clientPortaal");
		add(clientPortaal);
		clientPortaal.setVisible(ScreenitSession.get().getLoggedInAccount() instanceof Client);
		add(new Label("location.coloscopieCentrum.naam"));
		List<RedenAfspraakAfzeggen> choices = new ArrayList<>(Arrays.asList(RedenAfspraakAfzeggen.values()));
		choices.remove(RedenAfspraakAfzeggen.CLIENT_OVERLEDEN);
		final RadioChoice<RedenAfspraakAfzeggen> redenAfzeggen = new RadioChoice<RedenAfspraakAfzeggen>("redenAfzeggen", choices);
		redenAfzeggen.setChoiceRenderer(new EnumChoiceRenderer<RedenAfspraakAfzeggen>(redenAfzeggen));
		redenAfzeggen.setRequired(true);
		redenAfzeggen.setLabel(Model.of("Reden afzeggen"));
		redenAfzeggen.setPrefix("<label class=\"radio\">");
		redenAfzeggen.setSuffix("</label>");
		add(redenAfzeggen);

		Boolean rechtVoorBriefTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		CheckBox briefTegenhoudenCheckBox = ComponentHelper.newCheckBox("briefTegenhouden", briefTegenhouden);
		briefTegenhoudenCheckBox.setVisible(rechtVoorBriefTegenhouden);
		briefTegenhoudenCheckBox.setOutputMarkupPlaceholderTag(true);
		add(briefTegenhoudenCheckBox);
	}

	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK, getModelObject());
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_STATUS, AfspraakStatus.GEANNULEERD_VIA_INFOLIJN);
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN, briefTegenhouden.getObject());
		return opslaanObjecten;

	}

	public List<String> getOpslaanMeldingen()
	{
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		ColonIntakeAfspraak afspraak = getModelObject();
		ColonScreeningRonde ronde = afspraak.getColonScreeningRonde();
		String melding = String.format("coloscopie intake afspraak van %1$s in %2$s van %3$s afzeggen.", format.format(afspraak.getStartTime()), afspraak.getLocation().getName(),
			afspraak.getLocation().getColoscopieCentrum().getNaam());

		if (ronde.getOpenUitnodiging() == null)
		{
			melding += " Hiermee wordt de lopende ronde afgerond.";
		}
		return Arrays.asList(melding);
	}
}
